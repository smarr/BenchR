get_per_iteration_data <- function(benchmark_name, file_name, equal_columns, per_iter_columns, ...) {
  data4 <- load_metrics_file(benchmark_name, 2, file_name)
  data5 <- load_metrics_file(benchmark_name, 3, file_name)
  if (nrow(data4) != nrow(data5)) {
    cat(benchmark_name)
    cat(file_name)
  }
  assert_that(nrow(data4) == nrow(data5))  # if this does not hold, there is something wrong with the data or benchmark
  
  rename_map <- sapply(per_iter_columns, function(x) paste0(x, "5"))
  data5 <- rename(data5, rename_map)
  data <- merge(data4, data5, by = as.character(as.quoted(equal_columns)))
  ddply(data, equal_columns, transform,
        ...)
}

## Info on executed methods
get_method_data <- memoise(function(benchmark_name) {
  per_iter <- get_per_iteration_data(
    benchmark_name, "defined-methods", ~ Name, c("Executed", "Execution.Count"),
    PerIteration = Execution.Count5 - Execution.Count)
  per_iter <- subset(per_iter, Executed == "true")
  
  data.frame(Benchmark = benchmark_name,
             NumMethodsExecutedAtLeastOnce = nrow(per_iter),
             NumMethodsExecutedEachIteration = nrow(subset(per_iter, PerIteration > 0)))
})

## Info on classes with executed methods
get_class_data <- memoise(function(benchmark_name) {
  per_it <- get_per_iteration_data(
    benchmark_name, "defined-classes", ~ Class.Name + Source.Section, c("Methods.Executed"),
    PerIteration = Methods.Executed5 - Methods.Executed)
  per_it$Benchmark <- as.factor(benchmark_name)
  per_it
})

get_class_stats <- function(benchmark_name) {
  data <- get_class_data(benchmark_name)
  data.frame(
    Benchmark = as.factor(benchmark_name),
    NumClassesWithExecutedMethods = nrow(subset(data, Methods.Executed > 0)))
}

## Loop info
get_loop_data <- memoise(function(benchmark_name) {
  get_per_iteration_data(
    benchmark_name, "loops", ~ Source.Section + Loop.Activations, c("Num.Iterations"),
    PerIteration = Num.Iterations5 - Num.Iterations)
})

get_loop_stats <- memoise(function(benchmark_name) {
  data <- get_loop_data(benchmark_name)
  total <- subset(data, Loop.Activations == "TOTAL" & PerIteration > 0)
  data.frame(
    Benchmark = as.factor(benchmark_name),
    NumLoops  = nrow(total),
    LoopIterations = sum(total$PerIteration))
})

### Call Site Stats
get_callsite_data <- memoise(function(benchmark_name) {
  get_per_iteration_data(
    benchmark_name, "method-callsites", ~ Source.Section, c("Call.Count", "Num.Rcvrs", "Num.Targets"),
    PerIteration = Call.Count5 - Call.Count)
})

get_rcvr_callsite_stats <- memoise(function(benchmark_name) {
  per_iter <- subset(get_callsite_data(benchmark_name), Call.Count > 0 & Num.Rcvrs > 0)
  ddply(per_iter, ~ Num.Rcvrs, here(summarise),
        Benchmark    = as.factor(benchmark_name),
        NumCallSites = length(Source.Section),
        NumCalls     = sum(PerIteration))
})

get_target_callsite_stats <- memoise(function(benchmark_name) {
  per_iter <- subset(get_callsite_data(benchmark_name), Call.Count > 0 & Num.Rcvrs > 0)
  ddply(per_iter, ~ Num.Targets, here(summarise),
        Benchmark    = as.factor(benchmark_name),
        NumCallSites = length(Source.Section),
        NumCalls     = sum(PerIteration))
})

get_closure_application_data <- memoise(function(benchmark_name) {
  get_per_iteration_data(
    benchmark_name, "closure-applications", ~ Source.Section, c("Call.Count", "Num.Targets"),
    PerIteration = Call.Count5 - Call.Count)
})

get_closure_stats <- memoise(function(benchmark_name) {
  per_iter <- get_closure_application_data(benchmark_name)
  per_iter <- subset(per_iter, Num.Targets > 0)
  ddply(per_iter, ~ Num.Targets, here(summarise),
        Benchmark       = as.factor(benchmark_name),
        NumSites        = length(Source.Section),
        NumApplications = sum(PerIteration))
})

## Branch Data
get_branch_data <- memoise(function(benchmark_name) {
  per_iter <- get_per_iteration_data(
    benchmark_name, "branches", ~ Source.Section, c("TrueCnt", "FalseCnt", "Total"),
    TrueIter  = TrueCnt5  - TrueCnt,
    FalseIter = FalseCnt5 - FalseCnt,
    TotalIter = Total5    - Total,
    CHECK = (TrueCnt + FalseCnt) - Total,
    TruePer   = TrueCnt / Total,
    CHECK5 = (TrueCnt5 + FalseCnt5) - Total5,
    BiasRatio = max(TrueCnt, FalseCnt) / Total,
    BiasRatioIter = ifelse(Total5 - Total == 0, max(TrueCnt, FalseCnt) / Total, max(TrueCnt5 - TrueCnt, FalseCnt5 - FalseCnt) / (Total5 - Total)),
    TruePerIter   = (TrueCnt5 - TrueCnt) / (Total5 - Total))
  per_iter$Benchmark <- as.factor(benchmark_name)
  per_iter
})

get_branch_stats <- memoise(function(benchmark_name) {
  data <- get_branch_data(benchmark_name)
  data <- subset(data, Total > 0)
  data.frame(
    Benchmark       = as.factor(benchmark_name),
    Branches        = length(data$Source.Section),
    BranchesPerIter = sum(data$TotalIter),
    BiasRatio       = geometric.mean(data$BiasRatio),
    BiasRatioIter   = geometric.mean(data$BiasRatioIter))
})

get_array_allocation_data <- memoise(function(benchmark_name) {
  get_per_iteration_data(
    benchmark_name, "new-arrays", ~ Source.Section + Size, c("New.Arrays"),
    PerIteration = New.Arrays5 - New.Arrays)
})

get_array_allocation_stats <- memoise(function(benchmark_name) {
  data <- get_array_allocation_data(benchmark_name)
  data <- ddply(data, ~ Source.Section + Size, transform,
        AllocMem = Size * PerIteration)
  
  data.frame(
    Benchmark = as.factor(benchmark_name),
    Sites     = length(unique(data$Source.Section)),
    NumArrays = sum(data$PerIteration),
    AllocArrMem  = sum(data$AllocMem)
  )
})

get_object_allocation_data <- memoise(function(benchmark_name) {
  get_per_iteration_data(
    benchmark_name, "new-objects", ~ Source.Section + Number.of.Fields + Class, c("New.Objects"),
    PerIteration = New.Objects5 - New.Objects)
})

get_object_allocation_stats <- memoise(function(benchmark_name) {
  data <- get_object_allocation_data(benchmark_name)
  data <- ddply(data, ~ Source.Section, transform,
        AllocMem = Number.of.Fields * PerIteration)
  data.frame(
    Benchmark   = as.factor(benchmark_name),
    Sites     = length(unique(data$Source.Section)),
    NumObjects  = sum(data$PerIteration),
    AllocObjMem = sum(data$AllocMem))
})

get_field_access_data <- memoise(function(benchmark_name) {
  get_per_iteration_data(
    benchmark_name, "field-accesses", ~ Source.Section + Access.Type + Data.Type, c("Count"),
    PerIteration = Count5 - Count)
})

get_field_access_stats <- memoise(function(benchmark_name) {
  data <- get_field_access_data(benchmark_name)
  data <- subset(data, Data.Type == "ALL" & Count > 0)
  reads  <- subset(data, Access.Type == "read")
  writes <- subset(data, Access.Type == "write")
  data.frame(
    Benchmark = as.factor(benchmark_name),
    NumReadSites   = length(reads$Source.Section),
    NumWriteSites  = length(writes$Source.Section),
    NumReadSitesPerIter  = length(subset(reads, PerIteration > 0)$Source.Section),
    NumWriteSitesPerIter = length(subset(writes, PerIteration > 0)$Source.Section),
    NumFieldReads  = sum(reads$PerIteration),
    NumFieldWrites = sum(writes$PerIteration),
    FieldReadRatio = sum(reads$PerIteration) / (sum(reads$PerIteration) + sum(writes$PerIteration)))
})

get_local_access_data <- memoise(function(benchmark_name) {
  get_per_iteration_data(
    benchmark_name, "local-accesses", ~ Source.Section + Access.Type + Data.Type, c("Count"),
    PerIteration = Count5 - Count)
})

get_local_access_stats <- memoise(function(benchmark_name) {
  data <- get_local_access_data(benchmark_name)
  data <- subset(data, Data.Type == "ALL" & Count > 0)
  reads  <- subset(data, Access.Type == "read")
  writes <- subset(data, Access.Type == "write")
  data.frame(
    Benchmark = as.factor(benchmark_name),
    NumReadSites = length(reads$Source.Section),
    NumWriteSites = length(writes$Source.Section),
    NumReadSitesPerIter  = length(subset(reads, PerIteration > 0)$Source.Section),
    NumWriteSitesPerIter = length(subset(writes, PerIteration > 0)$Source.Section),
    NumVarReads  = sum(reads$PerIteration),
    NumVarWrites = sum(writes$PerIteration),
    VarReadRatio = sum(reads$PerIteration) / (sum(reads$PerIteration) + sum(writes$PerIteration)))
})

get_array_access_stats <- memoise(function(benchmark_name) {
  data <- get_operation_basic_data(benchmark_name)
  reads  <- subset(data, Cat == "ArrayRead")
  writes <- subset(data, Cat == "ArrayWrite")
  data.frame(
    Benchmark = as.factor(benchmark_name),
    NumReadSites = length(reads$Source.Section),
    NumWriteSites = length(writes$Source.Section),
    NumReadSitesPerIter  = length(subset(reads, PerIteration > 0)$Source.Section),
    NumWriteSitesPerIter = length(subset(writes, PerIteration > 0)$Source.Section),
    NumArrReads  = sum(reads$PerIteration),
    NumArrWrites = sum(writes$PerIteration),
    ArrReadRatio = ifelse(sum(reads$PerIteration) + sum(writes$PerIteration) == 0, 0, sum(reads$PerIteration) / (sum(reads$PerIteration) + sum(writes$PerIteration))))
})

# General Statistics
get_general_stats <- memoise(function(benchmark_name) {
  data4 <- load_metrics_file(benchmark_name, 2, "general-stats")
  data5 <- load_metrics_file(benchmark_name, 3, "general-stats")
  
  # if this does not hold, there is something wrong with the data or benchmark
  assert_that(nrow(data4) == nrow(data5))
  rownames(data4) <- data4[,1]
  rownames(data5) <- data5[,1]
  data4[,1] <- NULL
  data5[,1] <- NULL
  
  assert_that(data4["Max Stack Height",] == data5["Max Stack Height",])
  assert_that(all(data4 == data5))
  
  data.frame(
    Benchmark      = benchmark_name,
    MaxStackHeight = data4["Max Stack Height",],
    LinesLoaded    = data4["Lines Loaded",],
    LinesExecuted  = data4["Lines Executed",],
    LinesWithStatements = data4["Lines With Statements",])
})

get_structural_execution_stats <- memoise(function() {
  meth_data    <- ldply(benchmark_names, get_method_data)
  used_classes <- ldply(benchmark_names, get_class_stats)
  
  struct_data <- merge(meth_data, used_classes)
  struct_data
})

# Operation Mix
sum_same_type <- function(data) {
  r <- ddply(data, ~ Source.Section + Operation + Category + Type, summarise,
             Invocations = sum(Invocations))
  subset(r, Type != "TOTAL")
}

categories <- c("ArrayRead", "ArrayWrite", "OpClosureApplication", "OpArithmetic", "OpComparison", "OpLength", "StringAccess")
# c <- "OpArithmetic"
# x <- "debug-HALT debug-CALL OpArithmetic BasicPrimitiveOperation"
decode_category <- function(x) {
  matches <- ldply(categories, function(c) { data.frame(Cat = c, Found = grepl(c, x)) })
  
  if (1 != sum(matches$Found)) {
    cat("ERROR")
    print(x)
  }
  assert_that(1 == sum(matches$Found))  # make sure the category is unique
  m <- subset(matches, Found == TRUE)
  m$Cat
}

get_operation_basic_data <- memoise(function(benchmark_name) {
  ## wont use helper function here, because we pre-filter data from the file
  ## with sum_same_type function
  calculate_per_iteration_invocations <- function(data4, data5) {
    data5 <- rename(data5, c("Invocations" = "Invocations5"))
    data <- merge(data4, data5, by = c("Source.Section", "Operation", "Category", "Type"))
    ddply(data, ~ Source.Section + Operation + Category + Type, transform,
          PerIteration = Invocations5 - Invocations,
          Cat = decode_category(Category))
  }
  
  data4 <- sum_same_type(load_metrics_file(benchmark_name, 2, "operations"))
  data5 <- sum_same_type(load_metrics_file(benchmark_name, 3, "operations"))
  
  # if this does not hold, there is something wrong with the data or benchmark
  if (nrow(data4) != nrow(data5)) {
    cat("Failure in get_operation_basic_data: ")
    cat(benchmark_name)
  }
  assert_that(nrow(data4) == nrow(data5))
  calculate_per_iteration_invocations(data4, data5)
})

get_operation_type_data <- memoise(function(benchmark_name) {
  data <- get_operation_basic_data(benchmark_name)
  data <- subset(data, Type != "TOTAL" & Cat != "ArrayRead" & Cat != "ArrayWrite" & Cat != "OpClosureApplication") ## Filter out all the stuff that is handled separately
  data <- ddply(data, ~ Source.Section + Operation + Type + Cat, transform,
                Group = determine_group(Operation, Type, Cat))
  data <- ddply(data, ~ Source.Section + Operation + Type + Cat + Group, transform,
                GroupSimple = group_simple(as.character(Group)))
  
  ddply(data, ~ Operation + Type + Cat + Group, here(summarise),
        Benchmark          = as.factor(benchmark_name),
        Sites              = length(Source.Section),
        PerIteration       = sum(PerIteration),
        Group              = unique(Group),
        GroupSimple        = unique(GroupSimple))
})

determine_group <- function(op, type, category) {
  kind <- paste0(as.character(op), " ", as.character(type), " ", as.character(category))
  switch(kind,
         `not bool OpArithmetic` =,
         `+ int OpArithmetic`    =,
         `- int OpArithmetic`    =,
         `<< int OpArithmetic`   =,
         `>>> int OpArithmetic`  =,
         `abs int OpArithmetic`  =,
         `bitXor: int OpArithmetic` =,
         `as32BitSignedValue int OpArithmetic` =,
         `as32BitUnsignedValue int OpArithmetic` =,
         `& int OpArithmetic` = "bool, int: +, -, &, <<, >>>, ^, !",
         
         `* int OpArithmetic`    =,
         `rem: int OpArithmetic` =,
         `% int OpArithmetic`    =,
         `/ int OpArithmetic`    = "int: *, /, %, rem",
         
         `+ float OpArithmetic`  =,
         `- float OpArithmetic`  =,
         `* float OpArithmetic`  =,
         `asInteger float OpArithmetic` =,
         `// float OpArithmetic` = "float: +, -, *, //, round",
         
         `sin float OpArithmetic`  =,
         `sqrt float OpArithmetic` =,
         `cos float OpArithmetic`  = "float: sin, cos, sqrt",

         `= bool OpComparison` =,
         `< int OpComparison`  =,
         `> int OpComparison`  =,
         `<= int OpComparison` =,
         `>= int OpComparison` =,
         `= int OpComparison`  =,
         `<> int OpComparison` = "bool, int: <, >, =, <=, <>, >=",
         
         `< float OpComparison`  =,
         `> float OpComparison`  =,
         `<= float OpComparison` =,
         `>= float OpComparison` =,
         `= float OpComparison`  =,
         `<> float OpComparison` = "float: <, >, =, <=, <>, >=",
         
         `= Symbol OpComparison`  =,
         `<> Symbol OpComparison` =,
         `= ref OpComparison`     =,
         `== ref OpComparison`    = "ptr: ==, !=",
         
         `+ str OpArithmetic`  =,
         `<> str OpComparison` =,
         `= str OpComparison`  = "str: +, =, !=",
           
         `length str OpLength` =,
         `size arr OpLength`   = "str/arr: length",
         `substringFrom:to: str StringAccess` = "str: substring",
         "unknown")
}

group_simple <- function (detailed) {
  d_str <- as.character(detailed)
  switch(detailed,
         "bool, int: +, -, &, <<, >>>, ^, !" = "IntArith",
         "int: *, /, %, rem"                 = "IntCplx",
         "float: +, -, *, //, round"         = "FltArith",
         "float: sin, cos, sqrt"             = "FltCplx",
         "bool, int: <, >, =, <=, <>, >="    = "IntCmp",
         "float: <, >, =, <=, <>, >="        = "FltCmp",
         "ptr: ==, !="                       = "PtrCmp",
         "str: +, =, !="                     = "StrCmp",
         "str/arr: length"                   = "Length",
         "str: substring"                    = "SubStr",
         "unknown-group")
}

get_op_group_stats <- memoise(function(benchmark_name) {
  data <- get_operation_type_data(benchmark_name)
  ddply(data, ~ Benchmark + Group + GroupSimple, summarise,
        Sites = sum(Sites),
        PerIteration = sum(PerIteration))
})

get_operation_stats <- memoise(function(benchmark_name) {
  ## Get Data
  gen          <- get_general_stats(benchmark_name)
  rcvr_calls   <- get_rcvr_callsite_stats(benchmark_name)
  target_calls <- get_target_callsite_stats(benchmark_name)
  closures <- get_closure_stats(benchmark_name)
  
  branches <- get_branch_stats(benchmark_name)
  loops    <- get_loop_stats(benchmark_name)
  
  arr <- get_array_allocation_stats(benchmark_name)
  obj <- get_object_allocation_stats(benchmark_name)
  
  local_accesses <- get_local_access_stats(benchmark_name)
  field_accesses <- get_field_access_stats(benchmark_name)
  array_accesses <- get_array_access_stats(benchmark_name)
  ops <- get_op_group_stats(benchmark_name)
  
  # Prepare Data
  rcvr_calls <- ddply(rcvr_calls, ~ Benchmark, summarise,
                      MonoRcvrCalls     = NumCalls[Num.Rcvrs==1],
                      MonoRcvrCallSites = NumCallSites[Num.Rcvrs==1],
                      PolyRcvrCalls     = sum(NumCalls[Num.Rcvrs>1]),
                      PolyRcvrCallSites = sum(NumCallSites[Num.Rcvrs>1]))
  
  target_calls <- ddply(target_calls, ~ Benchmark, summarise,
                      MonoTrgtCalls     = NumCalls[Num.Targets==1],
                      MonoTrgtCallSites = NumCallSites[Num.Targets==1],
                      PolyTrgtCalls     = sum(NumCalls[Num.Targets>1]),
                      PolyTrgtCallSites = sum(NumCallSites[Num.Targets>1]))

  closures <- ddply(closures, ~ Benchmark, summarise,
                    MonoApplies      = ifelse(length(NumApplications[Num.Targets==1]) == 0, 0, NumApplications[Num.Targets==1]),
                    MonoClosureSites = ifelse(length(NumSites[Num.Targets==1]) == 0, 0, NumSites[Num.Targets==1]),
                    PolyApplies      = sum(NumApplications[Num.Targets>1]),
                    PolyClosureSites = sum(NumSites[Num.Targets>1]))
  arr <- rename(arr, c("Sites" = "ArrAllocSites"))
  obj <- rename(obj, c("Sites" = "ObjAllocSites"))
  
  local_accesses <- rename(local_accesses, c("NumReadSites" = "VarReadSites", "NumWriteSites" = "VarWriteSites", "NumReadSitesPerIter" = "VarReadSitesPerIter", "NumWriteSitesPerIter" = "VarWriteSitesPerIter"))
  field_accesses <- rename(field_accesses, c("NumReadSites" = "FldReadSites", "NumWriteSites" = "FldWriteSites", "NumReadSitesPerIter" = "FldReadSitesPerIter", "NumWriteSitesPerIter" = "FldWriteSitesPerIter"))
  array_accesses <- rename(array_accesses, c("NumReadSites" = "ArrReadSites", "NumWriteSites" = "ArrWriteSites", "NumReadSitesPerIter" = "ArrReadSitesPerIter", "NumWriteSitesPerIter" = "ArrWriteSitesPerIter"))
  
  ops <- melt(ops, id.vars=1:3)
  ops <- dcast(ops, Benchmark ~ GroupSimple + variable, value.var = "value")
  
  data <- merge(gen, rcvr_calls)
  data <- merge(data, target_calls)
  data <- merge(data, closures)
  data <- merge(data, branches)
  data <- merge(data, loops)
  data <- merge(data, arr)
  data <- merge(data, obj)
  data <- merge(data, local_accesses)
  data <- merge(data, field_accesses)
  data <- merge(data, array_accesses)
  data <- merge(data, ops)
  data
})

##
## Goal of the principal component analysis:
##   - determine the similarity of the benchmarks
##   - how do they relate to each other based on the different types of 
##     operations they perform
##
## How should I normalize the data?
## What does it tell us if I normalize the number of ops compared to the maximum observed in all benchmarks
## vs. comparing it to the relative proportion in a benchmark??
## I think the relative proportion within a benchmark is problematic
## because that would weight them equally, and we have no idea of the relative
## relevance of each of them
## so, scaling them compared to other benchmarks keeps these independent, which is probably good
##
## Attempt to be as detailed as possible, separate out all types of information
## take types into account.
## The PCA should reduce the dimensionalty to the most important aspects.
#
# Focus this data on the per-iteration data, i.e., the elements of
# the benchmarks that are hot, where possible.
get_all_data_normalized <- function() {
  normalize_col_by_max <- function(data, col) {
    data[col] <- data[col] / max(data[col])
    data
  }
  normalize_by_max <- function(data) {
    for (i in 2:ncol(data)) {
      data[i] <- data[i] / max(data[i])
    }
    data
  }
  
  # Normalize stack height globally
  stack <- ldply(benchmark_names, get_general_stats)
  stack$RelativeStackHeight = stack$MaxStackHeight / max(stack$MaxStackHeight)
  data <- stack[-2] # leave out absolute numbers
  
  # Do not include structural information for the moment
  # Normalize instruction hotness globally
  #hot <- ldply(benchmark_names, get_instruction_hotness_stats)
  #hot <- hot[, -grep("Cold", names(hot))]
  #data <- merge(data, normalize_by_max(hot))
  
  # Use geometric mean for the branch bias ratio, per benchmark
  branch_stats <- ldply(benchmark_names, get_branch_stats)
  branch_stats <- normalize_col_by_max(branch_stats, "BranchesPerIter")
  data <- merge(data, branch_stats[-3]) # leave out absolute numbers
  
  loop_stats <- ldply(benchmark_names, get_loop_stats)
  data <- merge(data, normalize_by_max(loop_stats))
  
  # Normalize the number of classes globally
  class_stats <- ldply(benchmark_names, get_class_stats)
  class_stats <- normalize_col_by_max(class_stats, "NumClassesWithExecutedMethods")
  data <- merge(data, class_stats)
  
  # From method data, use degree of hot methods, and normalize num. hot methods globally
  method_data <- ldply(benchmark_names, get_method_data)
  method_data <- ddply(method_data, ~ Benchmark, transform,
                       HotMethodPart = NumMethodsExecutedEachIteration / NumMethodsExecutedAtLeastOnce)
  method_data <- normalize_col_by_max(method_data, "NumMethodsExecutedEachIteration")
  data <- merge(data, method_data[-2]) # leave out absolute numbers
  
  # Take info about mono and polymorphic call sites
  callsites <- ldply(benchmark_names, get_callsite_stats)
  callsites <- ddply(callsites, ~ Benchmark, summarise,
                     MonomorphicCalls     = NumCalls[Num.Targets==1],
                     MonomorphicCallSites = NumCallSites[Num.Targets==1],
                     PolymorphicCalls     = sum(NumCalls[Num.Targets>1]),
                     PolymorphicCallSites = sum(NumCallSites[Num.Targets>1]))
  data <- merge(data, normalize_by_max(callsites))
  
  # Allocation statistics for arrays and objects
  arr_alloc <- ldply(benchmark_names, get_array_allocation_stats)
  data <- merge(data, normalize_by_max(arr_alloc))
  obj_alloc <- ldply(benchmark_names, get_object_allocation_stats)
  data <- merge(data, normalize_by_max(obj_alloc))
  
  # Accesses statistics for fields, locals, and arrays
  field_stats <- ldply(benchmark_names, get_field_access_stats)
  field_stats <- normalize_col_by_max(field_stats, "NumFieldReads")
  field_stats <- normalize_col_by_max(field_stats, "NumFieldWrites")
  data <- merge(data, field_stats)
  
  local_stats <- ldply(benchmark_names, get_local_access_stats)
  local_stats <- normalize_col_by_max(local_stats, "NumVarReads")
  local_stats <- normalize_col_by_max(local_stats, "NumVarWrites")
  data <- merge(data, local_stats)
  
  arr_stats <- ldply(benchmark_names, get_array_access_stats)
  arr_stats <- normalize_col_by_max(arr_stats, "NumArrReads")
  arr_stats <- normalize_col_by_max(arr_stats, "NumArrWrites")
  data <- merge(data, arr_stats)
  
  # Statistics on operations
  ops_data <- get_detailed_ops_per_benchmark()
  data <- merge(data, normalize_by_max(ops_data))

  # Statistics on closures
  closure <- ldply(benchmark_names, get_closure_stats)
  data <- merge(data, normalize_by_max(closure))
  data
}
