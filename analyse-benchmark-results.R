# Read in the benchmark data

source("~/Projects/PhD/IBM/notes/scheduling-issue/helper.R")

# Make sure Cores and Iterations are treated as a factors
## cores <- range(bench$Cores)
## allPossibleCores <- cores[1]:cores[2]
#bench$Cores <- factor(bench$Cores, ordered=TRUE)  ## disabled to have better scale on diagrams, should be used as actual integer value
#bench$Iterations <- factor(bench$Iterations, ordered=TRUE)

bench <- load_data()

# Separate Data for display

# TOOD: do the statistics on the data, get the mean, median, stddev, conf-interval for each
#       benchmark
norm_bench <- ddply(bench, ~ Benchmark + VirtualMachine + Platform + ExtraArguments + None + Criterion,
         transform,
         Time.norm = Time / Time[Cores == min(Cores)])


stats <- ddply(norm_bench, ~ Benchmark + VirtualMachine + Platform + ExtraArguments + Cores + None + Criterion,
         summarise,
         Time.mean=mean(Time),
         Time.stddev=sd(Time),
         Time.median=median(Time),
         Time.mean095Error=confInterval095Error(Time),
         Time.cnfIntHigh = mean(Time) + (confInterval095Error(Time)),
         Time.cnfIntLow = mean(Time) - (confInterval095Error(Time)),
         Time.mean.norm=mean(Time.norm),
         Time.stddev.norm=sd(Time.norm),
         Time.median.norm=median(Time.norm),
         Time.mean095Error.norm=confInterval095Error(Time.norm),
         Time.cnfIntHigh.norm = mean(Time.norm) + (confInterval095Error(Time.norm)),
         Time.cnfIntLow.norm = mean(Time.norm) - (confInterval095Error(Time.norm)),
         BId = interaction(Benchmark[1], ExtraArguments[1], drop=TRUE),
         name = bench_names[interaction(Benchmark[1], ExtraArguments[1], drop=TRUE)])


###splittedStats <- splitBy(formula = ~ Benchmark + Virtual.Machine + Platform + Extra.Arguments + None, data = stats)

## Which info do we want to get out of the data
### Specific
###   - does the thread-local stuff have any influence?
###    -> compare rvm-intel-8u to rvm-intel-8u-tl
###   - scheduler bottleneck
###    -> compare int/float loop benchs with many and few processes, look at Chameleons benchmark
### General Performance
###  - do we have weak scaling? how does the  cores/(work/time) factor develop? ()
###  - how to present that best? how to show the different platforms?
###  - redo usual Excel graphs first


### Thread-local Experiment
# prepare data
threadlocal_bench <- subset(bench, VirtualMachine == "rvm-intel-8u" | VirtualMachine == "rvm-intel-8u-tl")
tl_speedup <- ddply(threadlocal_bench, ~ Benchmark + Cores + Platform + ExtraArguments + None + Criterion,
         transform,
         Time.speed = Time / Time[VirtualMachine == "rvm-intel-8u"])

stats <- ddply(tl_speedup, ~ Benchmark + VirtualMachine + Platform + ExtraArguments + Cores + None + Criterion,
         summarise,
         Time.mean=mean(Time.speed),
         Time.stddev=sd(Time.speed),
         Time.median=median(Time.speed),
         Time.mean095Error=confInterval095Error(Time.speed),
         Time.cnfIntHigh = mean(Time.speed) + (confInterval095Error(Time.speed)),
         Time.cnfIntLow = mean(Time.speed) - (confInterval095Error(Time.speed)),
         BId = interaction(Benchmark[1], ExtraArguments[1], VirtualMachine[1], drop=TRUE))

# filter out baseline
stats <- subset(stats, VirtualMachine == "rvm-intel-8u-tl")

# drop unused factor levels
stats <- dropUnusedFactors(stats)



## experiment with a plot for the tl experiment
p <- ggplot(stats, aes(Cores,
                       Time.mean,
                       group = BId,
                       colour = BId)) +
     geom_line(size = 1)     
p

# Now the scheduler bottleneck
###    -> compare int/float loop benchs with many and few processes, look at Chameleons benchmark
scheduler_bench <- subset(bench, 
                          (VirtualMachine == "rvm-intel-8u" | VirtualMachine == "rvm-tilera")
                          & (Benchmark == "SMarkLoops.benchFloatLoop" | Benchmark == "SMarkLoops.benchIntLoop" |
                             Benchmark == "BenchmarkGameSuite.benchChameleons"))
#tl_speedup <- ddply(threadlocal_bench, ~ Benchmark + Cores + Platform + ExtraArguments + None + Criterion,
#         transform,
#         Time.speed = Time / Time[VirtualMachine == "rvm-intel-8u"])

stats <- ddply(scheduler_bench, ~ Benchmark + VirtualMachine + Platform + ExtraArguments + Cores + None + Criterion,
         summarise,
         Time.mean=mean(Time),
         Time.stddev=sd(Time),
         Time.median=median(Time),
         Time.mean095Error=confInterval095Error(Time),
         Time.cnfIntHigh = mean(Time) + (confInterval095Error(Time)),
         Time.cnfIntLow = mean(Time) - (confInterval095Error(Time)),
         BId = interaction(Benchmark[1], ExtraArguments[1], VirtualMachine[1], drop=TRUE))

stats_8u <- dropUnusedFactors( subset(stats, VirtualMachine == "rvm-intel-8u") )
stats_tilera <- dropUnusedFactors( subset(stats, VirtualMachine == "rvm-intel-8u") )

p <- ggplot(stats_8u, aes(Cores,
                       Time.mean,
                       group = BId,
                       colour = BId)) +
     geom_line(size = 1) +
     geom_errorbar(aes(ymin=Time.cnfIntLow, ymax = Time.cnfIntHigh)) +
     geom_point(aes(shape=factor(BId), size=4)) +
     scale_shape_manual("",values=c("BenchmarkGameSuite.benchChameleons.None.rvm-intel-8u"=1,
                                    "BenchmarkGameSuite.benchChameleons.None.rvm-tilera"=2,
                                    "SMarkLoops.benchFloatLoop.%(cores)s 9.rvm-intel-8u"=3,
                                    "SMarkLoops.benchFloatLoop.%(cores)s0 5.rvm-intel-8u"=4,
                                    "SMarkLoops.benchFloatLoop.%(cores)s 1.rvm-tilera"=5,
                                    "SMarkLoops.benchFloatLoop.%(cores)s0 1.rvm-tilera"=6,
                                    "SMarkLoops.benchIntLoop.%(cores)s 30.rvm-intel-8u"=7,
                                    "SMarkLoops.benchIntLoop.%(cores)s0 5.rvm-intel-8u"=8,
                                    "SMarkLoops.benchIntLoop.%(cores)s 3.rvm-tilera"=9,
                                    "SMarkLoops.benchIntLoop.%(cores)s0 1.rvm-tilera"=10    )) +
    scale_colour_manual("",values=c("BenchmarkGameSuite.benchChameleons.None.rvm-intel-8u"=1,
                                    "BenchmarkGameSuite.benchChameleons.None.rvm-tilera"=2,
                                    "SMarkLoops.benchFloatLoop.%(cores)s 9.rvm-intel-8u"=3,
                                    "SMarkLoops.benchFloatLoop.%(cores)s0 5.rvm-intel-8u"=4,
                                    "SMarkLoops.benchFloatLoop.%(cores)s 1.rvm-tilera"=5,
                                    "SMarkLoops.benchFloatLoop.%(cores)s0 1.rvm-tilera"=6,
                                    "SMarkLoops.benchIntLoop.%(cores)s 30.rvm-intel-8u"=7,
                                    "SMarkLoops.benchIntLoop.%(cores)s0 5.rvm-intel-8u"=8,
                                    "SMarkLoops.benchIntLoop.%(cores)s 3.rvm-tilera"=9,
                                    "SMarkLoops.benchIntLoop.%(cores)s0 1.rvm-tilera"=10    ))
p

# filter to first 16 cores
stats <- subset(stats, Cores <= 16)

# drop unused factor levels
stats[] <- lapply(stats, function(x) if (is.factor(x)) factor(x) else x)



# Get some basic infos for the plot
xrange <- 1:16
yrange <- range(stats$Time.mean, stats$Time.cnfIntHigh, stats$Time.cnfIntLow)
nbench <- length(levels(stats$BId)) ##length(splittedStats)

p <- qplot(Cores, Time.mean, colour=BId, data=stats) +    # BId - the bechmark id is the classifiying property, and brings the color
     geom_errorbar(aes(ymin=Time.cnfIntLow, ymax = Time.cnfIntHigh)) +
     geom_line() +
     geom_point(size=3.5)
     #geom_point(aes(shape=BId, fill=BId), size=5) # to many items :(
p

p2 <- qplot(Cores, Time.mean.norm, colour=BId, data=stats) +    # BId - the bechmark id is the classifiying property, and brings the color
     geom_errorbar(aes(ymin=Time.cnfIntLow.norm, ymax = Time.cnfIntHigh.norm)) +
     geom_line() +
     geom_point(size=3.5)
     #geom_point(aes(shape=BId, fill=BId), size=5) # to many items :(
p2

### Below only old code 

# colors <- rainbow(nbench)
# linetype <- c(1:nbench) 
# plotchar <- seq(18, 18+nbench, 1)

# plot(xrange, yrange, type="n", xlab="Cores", ylab="Time (ms)")

# Simple plots, not ideal since the data was not aggregated before
# for (i in 1:nbench) { 
#  benchPart <- splittedBench[i]
#  name <- names(benchPart)
#  bp <- benchPart[[name]]
#  stats <- ddply(bp, ~Cores, summarise, mean=mean(Time), stddev=sd(Time), median=median(Time))
#  lines(stats$Cores, stats$mean, type="b", lwd=1.5, lty=linetype[i], col=colors[i], pch=plotchar[i]) 
# }
#  p <- qplot(bp$Cores, bp$Time, bp)
# p + geom_line()
# stats <- ddply(bp, ~Cores, summarise, mean=mean(Time), stddev=sd(Time), median=median(Time))
# qplot(stats$Cores, stats$mean, stats) + geom_line() + geom_errorbar(aes(ymin = stats$mean - stats$stddev, ymax = stats$mean + stats$stddev))


