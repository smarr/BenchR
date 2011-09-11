# Goal: compare for every benchmark the VMADL version to the handcrafted version
# then build the geometrical average, for one specific VM configuration
# then build the overall average VMADL vs. handcrafted (and always maintain the conf interval)

# Load libraries
setwd("~/Projects/PhD/CSOMPL/jot-csompl/benchmarks")
source("helper.R")

# Read in the benchmark data
bench <- load_data_file("test.csv")
bench <- load_data_file("test-hand.csv")

# We only care about a subset of the variables
bench <- subset(bench, select=c(Time, Benchmark, VirtualMachine))

# Make it easier to know the actual factors
bench <- ddply(bench, ~ Benchmark + VirtualMachine,
               transform,
               Approach = factor(tail(strsplit(as.character(VirtualMachine), "-")[[1]], 1)),
               Configuration = factor(sub("(-vmadl)|(-handcrafted)", "", as.character(VirtualMachine))))
bench <- subset(bench, select=c(Time, Benchmark, Configuration , Approach))

bench <- orderBy(~Time + Benchmark + Configuration + Approach, bench)



benchAvgConfig  <- bench
benchAllConfigs <- bench

# make all measurements part of the same configuration, our avg config will give us a total result on the plot
benchAvgConfig$Configuration <- factor("avg")

# now combine them again
bench <- rbind(benchAvgConfig, benchAllConfigs)

benchSavedScripted <- bench
benchSavedManual <- bench

bench <- benchSavedScripted

### Some filters for experimenting:
## bench <- subset(bench, (Benchmark != "IntegerLoop" | Configuration != "plain" ) & (Benchmark != "Fibonacci" | Configuration != "plain" ))
## bench <- subset(bench, Configuration == "native") #filter one specific configuration
## bench <- subset(bench, Configuration != "image") #filter broken run

# Calculate the ratio: vmadl/handcrafted
normalized_data <- ddply(bench, ~ Benchmark + Configuration,
      transform,
      SpeedRatio = Time / Time[Approach == "handcrafted"])

# Now just drop all the stuff we do not need
normalized_data <- subset(normalized_data, Approach == "vmadl", c(SpeedRatio, Benchmark, Configuration))
normalized_data <- drop_unused_factors(normalized_data)

normalizedScript <- normalized_data
normalizedManual <- normalized_data
#normAvgConfig  <- normalized_data
#normAllConfigs <- normalized_data

# make all measurements part of the same configuration, our avg config will give us a total result on the plot
#normAvgConfig$Configuration <- factor("avg")

# now combine them again
#normalized_data <- rbind(normAvgConfig, normAllConfigs)



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

summary(stats)
summary(normalized_data)
summary(bench)

summary(subset(normalized_data, Configuration == "image"))

# Bean plot for the different configurations
pdf(file="gcc-Os-flto-bean.pdf", width=11, height=5)
beanplot(SpeedRatio ~ Configuration,
         data = normalized_data,
         what = c(1,1,1,0),
         #main="Performance Comparison",
         log="y", # don't use log-scale
         ylab="Runtime: VMADL/handcrafted",
         #cutmin=0.8, # allows to cut off outliers, but more like lying...
         #cutmax=1.2,
         ylim=c(0.95, 1.05),
         #las=2,
         #par(mgp=c(3,2,0)),
         names=c("total", "green", "marksweep\nimage", "marksweep\nint-one",
                 "marksweep", "native", "plain", "refcount", "marksweep\nthreading")
         )

# add an additional line for the expected value
yline(1, lty="longdash")
yline(1.01, lty="solid", col="lightgray")
yline(1.02, lty="solid", col="lightgray")
yline(1.03, lty="solid", col="lightgray")
yline(1.04, lty="solid", col="lightgray")
yline(1.05, lty="solid", col="lightgray")

yline(0.99, lty="solid", col="lightgray")
yline(0.98, lty="solid", col="lightgray")
yline(0.97, lty="solid", col="lightgray")
yline(0.96, lty="solid", col="lightgray")
yline(0.95, lty="solid", col="lightgray")

dev.off()

pdf(file="gcc-Os-flto-box.pdf", width=11, height=5)
boxplot(SpeedRatio ~ Configuration, 
        data = normalized_data, 
        ylab="Runtime: VMADL/handcrafted",
        ylim=c(0.95, 1.05),
        names=c("total", "green", "marksweep\nimage", "marksweep\nint-one",
           "marksweep", "native", "plain", "refcount", "marksweep\nthreading"))
yline(1, lty="longdash")
yline(1.01, lty="solid", col="lightgray")
yline(1.02, lty="solid", col="lightgray")
yline(1.03, lty="solid", col="lightgray")
yline(1.04, lty="solid", col="lightgray")
yline(1.05, lty="solid", col="lightgray")

yline(0.99, lty="solid", col="lightgray")
yline(0.98, lty="solid", col="lightgray")
yline(0.97, lty="solid", col="lightgray")
yline(0.96, lty="solid", col="lightgray")
yline(0.95, lty="solid", col="lightgray")
dev.off()

