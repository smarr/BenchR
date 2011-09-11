#!/usr/bin/env Rscript

### Analyse the impact of the use of an object table and extra object header
### words on multicore performance

# set working directory and load libraries
setwd("~/Projects/BenchR/")
source("libs/helper.R")

row_names <- c("Time", "Benchmark", "VirtualMachine", "Platform",
                  "ExtraArguments", "Cores", "Iterations", "None", "Criterion", "Criterion-total")
bench <- load_data_file("~/Projects/PhD/IBM/bench-ot/results.data.csv", row_names)

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

# We assume that all variations in measurements come from the same
# non-deterministic influences.
# This allows to order the measurements, before corelating them pair-wise.
bench <- orderBy(~Time + Platform + ObjectTable + Header + Benchmark + ExtraArguments + Cores, bench)

# Now on to question 1
ot_data <- subset(bench, Header != "no")

# Calculate the performance ratio
norm_ot_data <- ddply(ot_data, ~ Platform + Header + Benchmark + ExtraArguments + Cores,
      transform,
      SpeedRatio = Time / Time[ObjectTable == TRUE])
# Now just drop all the stuff we do not need
norm_ot_data <- subset(norm_ot_data, ObjectTable == FALSE, c(SpeedRatio, Platform, Header, Benchmark, ExtraArguments, Cores))
norm_ot_data <- drop_unused_factors(norm_ot_data)

pdf(file="Cores1-full-header-OTimpact.pdf", width=40, height=20)

beanplot(SpeedRatio ~ Cores + Header,
         data = norm_ot_data,
         what = c(1,1,1,0),
         #main="Performance Comparison",
         log="", # don't use log-scale
         ylab="Runtime: noOT/OT",
         #cutmin=0.8, # allows to cut off outliers, but more like lying...
         #cutmax=1.2,
         #ylim=c(0.95, 1.05),
         las=2,
         #par(mgp=c(3,2,0)),
         ignored_variable="at_the_end")
dev.off()

beanplot(SpeedRatio ~ Benchmark,
         data = subset(norm_ot_data, Cores == 1 & Header == "half"),
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

die-here  ###---->>> below old script

# TODO: - look at data from 1-core only, bar chart?
#         and 8 cores only
#       - calculate the average with 1 core, with 8, and completely

## Drop some not so useful benchmarks
## We are not interested in the scheduler benchmarks, and the single core slowdown problem
#bench_filtered <- drop_unused_factors(subset(bench, 
#                                               (ExtraArguments != "%(cores)s0 1")
#                                             & (ExtraArguments != "%(cores)s0 4")
#                                             & (ExtraArguments != "1 30")
#                                             & (ExtraArguments != "1 40")
#                                             & (ExtraArguments != "1 5")
#                                             ))

## Normalize data to variant with object-table VirtualMachine == with-ot
# other way to say how to spilt/group the data: .(Benchmark, VirtualMachine, ExtraArguments, Cores)
# REM: make sure the factor for normalization is not in the list of grouping factors
#norm_bench <- ddply(bench_filtered, ~ Benchmark + ExtraArguments + Cores,
#                    transform,
#                    Time.norm = Time / Time[VirtualMachine == 'with-ot'])   ## Normalize to 1-core runtime per benchmark

## Add all the necessary statistic values
#stats <- ddply(norm_bench, ~ Benchmark + VirtualMachine + Platform + ExtraArguments + Cores + None + Criterion,
#         summarise,
         #Time.mean=mean(Time),  # will rely on the normalized values only
         #Time.stddev=sd(Time),
         #Time.median=median(Time),
         #Time.mean095Error=confInterval095Error(Time),
         #Time.cnfIntHigh = mean(Time) + (confInterval095Error(Time)),
         #Time.cnfIntLow = mean(Time) - (confInterval095Error(Time)),
#         Time.mean.norm=mean(Time.norm),
#         Time.stddev.norm=sd(Time.norm),
#         Time.median.norm=median(Time.norm),
#         Time.mean095Error.norm=confInterval095Error(Time.norm),
#         Time.cnfIntHigh.norm = mean(Time.norm) + (confInterval095Error(Time.norm)),
#         Time.cnfIntLow.norm = mean(Time.norm) - (confInterval095Error(Time.norm)),
#         BId = interaction(Benchmark[1], ExtraArguments[1], drop=TRUE))

#bench_for_bean <- ddply(bench, ~ Benchmark + ExtraArguments + VirtualMachine,
#         transform,
#         BId = interaction(Benchmark, ExtraArguments, VirtualMachine, drop=TRUE))

stats <- ddply(bench, ~ Benchmark + VirtualMachine + Platform + ExtraArguments + Cores + None + Criterion,
         summarise,
         Time.mean=mean(Time),  # will rely on the normalized values only
         Time.stddev=sd(Time),
         Time.median=median(Time),
         Time.mean095Error=confInterval095Error(Time),
         Time.cnfIntHigh = mean(Time) + (confInterval095Error(Time)),
         Time.cnfIntLow = mean(Time) - (confInterval095Error(Time)),
         BId = interaction(Benchmark[1], ExtraArguments[1], drop=TRUE))

#~ Benchmark + ExtraArguments + VirtualMachine
#bench_sum <- ddply(bench,  ~ Benchmark + ExtraArguments + VirtualMachine, #~ Benchmark + VirtualMachine + Platform + ExtraArguments + Cores + None + Criterion,
#         transform,
#         Time.norm = Time / Time[VirtualMachine == 'with-ot'], drop=TRUE)


#complete_stats <- ddply(stats, ~ VirtualMachine, summarize,
#        Time.mean=mean(Time.mean),  # will rely on the normalized values only
#        Time.stddev=sd(Time.mean))

# Graph 1 - Intel showing weak scalability

# filter out intel data
#intel_data <- drop_unused_factors(subset(stats, VirtualMachine == "without-ot-with-backpointer"))
#intel_scale <- custom_scale(intel_data$BId)
#intel_data <- drop_unused_factors(stats)
#intel_scale <- custom_scale(intel_data$BId)


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


