## This file defines common functions used for data processing.

load_data_file <- function (file, row_names) {
  if (missing(row_names)) {
    # these row names are hard coded and might not be applicable
    # names like this are generated by ReBench
    row_names <- c("TimeStamp", "Value", "Unit", "Criterion", "Benchmark",
                   "VM", "Suite", "Extra", "Warmup", "Cores", "InputSize",
                   "Var")
  }
  
  bench <- read.table(file, sep="\t", header=FALSE, col.names=row_names, fill=TRUE)
  bench$rid = seq_len(nrow(bench))
  bench <- ddply(bench, ~ Benchmark + VM + Var + Extra + Cores + Suite, transform,
                 Iteration = rid - min(rid))
  bench
}


load_savina_data_file <- function (file, name = "ScalaAkka") {
  data = load_data_file(file)
  data$VM = revalue(data$VM, c("java" = name))
  data$Benchmark = revalue(data$Benchmark,
                           c("AllPairsShortestPath" = "SavinaApsp",
                             "RadixSort" = "SavinaRadixSort",
                             "TrapezoidalApproximation" = "SavinaTrapezoidal"))
  data
}

prepare_vm_names <- function(data) {
  name_map <-     list("Java8U66"              = "Java",
                       "Node"                  = "Node.js",
                       "JRubyTruffleEnterprise"= "JRuby+Truffle",
                       "RPySOM-jit"            = "RPySOM",
                       "RTruffleSOM-jit"       = "RTruffleSOM",
                       "SOMns-jit"             = "SOMns",
                       
                       "TruffleSOM-graal"      = "TruffleSOM",
                       "TruffleSOM-graal-no-split" = "TruffleSOM.ns",
                       "SOMpp"                 = "SOM++")
  # Rename
  levels(data$VM)  <- map_names(
    levels(data$VM),
    name_map)
  data
}

map_names <- function(old_names, name_map) {
  for (i in 1:length(old_names)) {
    old_name <- old_names[[i]]
    if (!is.null(name_map[[old_name]])) {
      old_names[i] <- name_map[[old_name]]
    }
  }
  old_names
}
