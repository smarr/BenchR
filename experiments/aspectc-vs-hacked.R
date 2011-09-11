# Goal: compare for every benchmark the VMADL version to the handcrafted version
# then build the geometrical average, for one specific VM configuration
# then build the overall average VMADL vs. handcrafted (and always maintain the conf interval)

# Load libraries
setwd("~/Projects/PhD/CSOMPL/jot-csompl/benchmarks")
source("helper.R")

# Read in the benchmark data
bench <- load_data_file("rebench.data.csv")

# We only care about a subset of the variables
bench <- subset(bench, select=c(Time, Benchmark, VirtualMachine))
bench <- orderBy(~Time + Benchmark + VirtualMachine, bench)

# Make it easier to know the actual factors
bench <- ddply(bench, ~ Benchmark + VirtualMachine,
               transform,
               Approach = factor(tail(strsplit(as.character(VirtualMachine), "-")[[1]], 1)),
               #Configuration = factor(sub("(-vmadl)|(-handcrafted)", "", as.character(VirtualMachine))))
               Configuration = factor("native"))

bench <- subset(bench, select=c(Time, Benchmark, Configuration , Approach))

## bench <- subset(bench, Configuration != "image") #filter broken run

# Calculate the ratio: vmadl/handcrafted
normalized_data <- ddply(bench, ~ Benchmark + Configuration,
      transform,
      SpeedRatio = Time / Time[Approach == "hacked"])

# Now just drop all the stuff we do not need
normalized_data <- subset(normalized_data, Approach == "vmadl", c(SpeedRatio, Benchmark, Configuration))
normalized_data <- drop_unused_factors(normalized_data)

# Summarize the date for the benchmarks
stats <- ddply(normalized_data, ~ Configuration,   # ~ Benchmark + Configuration
               summarise,
               
               SpeedRatio.mean=geometric.mean(SpeedRatio),
               SpeedRatio.stddev=sd(SpeedRatio),
               SpeedRatio.median=median(SpeedRatio),
               SpeedRatio.arithMean = mean(SpeedRatio),
               SpeedRatio.mean095Error=confInterval095Error(SpeedRatio),
               SpeedRatio.cnfIntHigh = mean(SpeedRatio) + (confInterval095Error(SpeedRatio)),
               SpeedRatio.cnfIntLow = mean(SpeedRatio) - (confInterval095Error(SpeedRatio)))

# Bean plot for the different configurations
#pdf(file="configurations-150runs-gcc44-Os-inlining.pdf", width=20, height=10)
beanplot(SpeedRatio ~ Configuration, data = normalized_data, what=c(1,1,1,0), main="Configuration Speedup", ylab="Speedup: VMADL/handcrafted", log="")
#dev.off()

boxplot(SpeedRatio ~ Configuration, data = normalized_data, main="Configuration Speedup", ylab="Speedup: VMADL/handcrafted")


