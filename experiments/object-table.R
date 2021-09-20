#!/usr/bin/env Rscript

### Analyse the impact of the use of an object table and extra object header
### words on multicore performance

# set working directory and load libraries
setwd("~/Projects/BenchR/")
source("libs/helper.R")

row_names <- c("Time", "Benchmark", "VirtualMachine", "Platform",
                  "ExtraArguments", "Cores", "Iterations", "None", "Criterion", "Criterion-total")
bench <- rbind(load_data_file("~/Projects/PhD/IBM/bench-ot/results.data.csv", row_names),
               load_data_file("~/Projects/PhD/IBM/bench-ot/results.data.tilera.csv", row_names))


# Questions to be answered:
#  1. What is the performance impact of using an object table?
#     -> relevant data: *-noOT-full-header vs. *-OT-full-header
#                   and *-noOT-half-header vs. *-OT-half-header
#     - does the number of header words influence it?
#     - is it specific to benchmarks?
#  2. What is the performance impact of adding header words?
#     -> relevant data: *-noOT-full-header vs. *-noOT-half-header vs. *-noOT-no-header
#                   and *-OT-full-header vs. *-OT-half-header

### Some general preprocessing

# Parse the data for better handling
# - separate by OT and noOT, and by header chracteristics
bench <- ddply(bench, ~ VirtualMachine,
               transform,
               ObjectTable = strsplit(as.character(VirtualMachine), "-")[[1]][2] == "OT",
               Header = factor(strsplit(as.character(VirtualMachine), "-")[[1]][3]))

### !!! Remove all subcriteria, currently that is for the NPBIS benchmark only anyway
bench <- subset(bench, Criterion == "total")

# We only care about a subset of the variables, the rest contains nothing interesting
bench <- subset(bench, select=c(Time, Platform, ObjectTable, Header,
                                Benchmark, ExtraArguments, Cores))

# We also are only intersted in the real weak-scaling benchmarks
norm_bench <- subset(norm_bench,
                     !grepl("^1 ", ExtraArguments)    # those beginning with "1 " put load on a single core
                     & !grepl("s0 ", ExtraArguments)) # those having "s0 " in it put 10x load on each core
norm_bench <- droplevels(norm_bench)


# We assume that all variations in measurements come from the same
# non-deterministic influences.
# This allows to order the measurements, before corelating them pair-wise.
bench <- orderBy(~Time + Platform + ObjectTable + Header + Benchmark + ExtraArguments + Cores, bench)

# Now on to Question 1
ot_data <- subset(bench, Header != "no")

# Calculate the performance ratio
norm_ot_data <- ddply(ot_data, ~ Platform + Header + Benchmark + ExtraArguments + Cores,
      transform,
      SpeedRatio = Time / Time[ObjectTable == TRUE])
# Now just drop all the stuff we do not need
norm_ot_data <- subset(norm_ot_data, ObjectTable == FALSE, c(SpeedRatio, Platform, Header, Benchmark, ExtraArguments, Cores))
norm_ot_data <- drop_unused_factors(norm_ot_data)

pdf(file="Cores1-full-header-OTimpact.pdf", width=40, height=20)

beanplot(SpeedRatio ~ Platform + Cores + Header,
         data = norm_ot_data,
         what = c(1,1,1,0),
         #main="Performance Comparison",
         log="y", # don't use log-scale
         ylab="Runtime: noOT/OT",
         #cutmin=0.8, # allows to cut off outliers, but more like lying...
         #cutmax=1.2,
         #ylim=c(0.95, 1.05),
         las=2,
         #par(mgp=c(3,2,0)),
         side="both",
         col = list("black", c("grey", "white")),
         ignored_variable="at_the_end")
dev.off()

beanplot(SpeedRatio ~ Platform + Benchmark,
         data = subset(norm_ot_data, Header == "full" & Platform == "tilera"), # Cores > 16 &
         what = c(1,1,1,0),
         #main="Performance Comparison",
         log="", # don't use log-scale
         ylab="Runtime: VMADL/handcrafted",
         #cutmin=0.8, # allows to cut off outliers, but more like lying...
         #cutmax=1.2,
         #ylim=c(0.95, 1.05),
         las=2,
         #par(mgp=c(3,2,0)),
         side="both",
         col = list("black", c("grey", "white")),
         ignored_variable="at_the_end")

# Question 2
#  2. What is the performance impact of adding header words?
#     -> relevant data: *-noOT-full-header vs. *-noOT-half-header vs. *-noOT-no-header
#                   and *-OT-full-header vs. *-OT-half-header

# Calculate the performance ratio
norm_header_data <- ddply(bench, ~ Platform + ObjectTable + Benchmark + ExtraArguments + Cores,
      transform,
      SpeedRatio = Time / Time[Header == "full"])
# Now just drop all the stuff we do not need
norm_header_data <- subset(norm_header_data, Header != "full",
                           c(SpeedRatio, Platform, ObjectTable, Header,
                             Benchmark, ExtraArguments, Cores))
norm_header_data <- drop_unused_factors(norm_header_data)

beanplot(SpeedRatio ~ ObjectTable + Header,
         data = subset(norm_header_data, Cores == 1),
         what = c(1,1,1,0),
         #main="Performance Comparison",
         log="", # don't use log-scale
         ylab="Runtime: VMADL/handcrafted",
         #cutmin=0.8, # allows to cut off outliers, but more like lying...
         #cutmax=1.2,
         #ylim=c(0.95, 1.05),
         las=2,
         #par(mgp=c(3,2,0)),
         ignored_variable="at_the_end")


die-here # -- old code below!!!!


create_line_plot_over_cores <- function(data, benchId, folder) {
  bench_data <- drop_unused_factors(subset(data, BId == benchId))
  #bench_scale <- custom_scale(bench_data$BId)
  p <- ggplot(bench_data, aes(Cores, Time.mean, group = VirtualMachine, colour = VirtualMachine))
  p <- p + geom_line(size = 1)
  p <- p + geom_errorbar(aes(ymin=Time.cnfIntLow, ymax = Time.cnfIntHigh))
  #p <- p + scale_shape_manual("Benchmarks", values=bench_scale)
  #p <- p + scale_colour_manual("Benchmarks",values=bench_scale)
  p <- p + geom_point(aes(shape=VirtualMachine, size=4))
  p <- p + xlab("Cores") + ylab("Runtime in ms")
  p <- p + opts(title=paste("Intel - 8 cores (16 hyperthreads): ", benchId, sep=""))
  #p <- p + opts(legend.position="bottom")
  p <- p + scale_x_continuous(breaks=1:16)
  p
  
  fileName <- paste(folder, Rdoc$escapeRdFilename(benchId), ".pdf", sep="")
  print(fileName)
  fileName <- str_replace_all(fileName, "[%()]", "_")
  print(fileName)
  ggsave(filename=fileName, plot=p)
  p
}

for (benchId in levels(stats$BId)) {
  create_line_plot_over_cores(stats, benchId, "~/Projects/PhD/IBM/notes/scheduling-issue/obj-table_")
}
#p <- create_line_plot_over_cores(stats, levels(stats$BId)[1], "~/Projects/PhD/IBM/notes/scheduling-issue/obj-table_")
#p


## Create beanplots for the result for a specific core number
create_beanplot <- function(data, cores, benchmark, extraArgs, folder) {
  fileName <- paste(folder, benchmark, '.c', cores, '.', Rdoc$escapeRdFilename(extraArgs), ".pdf", sep="")
  fileName <- str_replace_all(fileName, "[%()]", "_")
  
  benchmark_name = paste(benchmark, " Cores: ", cores, " ExtraArgs: ", extraArgs, sep="")
  
  pdf(file=fileName, width=10, height=10)
  tryCatch(beanplot(Time ~ VirtualMachine, data = data, main= benchmark_name, ylab="Runtime in ms"),
           error = function(e) e, finally=function(){dev.off()})
  dev.off()
}


#bench_data <- bench_for_bean
#bench_data <- drop_unused_factors(subset(bench_data, (Cores == 1) & (Benchmark == 'SMarkCompiler') & (ExtraArguments == '%(cores)s 50') ))
#p <- beanplot(Time ~ BId, data = bench_data)
 

for (coreNumber in 1:8) {
  print(coreNumber)
  core_data <- drop_unused_factors(subset(bench, (Cores == coreNumber)))
  for (benchmark in levels(core_data$Benchmark)) {
    b_data <- drop_unused_factors(subset(core_data, (Benchmark == benchmark)))
    for (extraArg in levels(b_data$ExtraArguments)) {
      ea_data <- drop_unused_factors(subset(b_data, (ExtraArguments == extraArg)))
      create_beanplot(ea_data, coreNumber, benchmark, extraArg, "~/Projects/PhD/IBM/notes/scheduling-issue/ot-beans_")
    }
  }
}


