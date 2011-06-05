# make sure the required library is loaded
library(doBy)
library(ggplot2)

# Read in the benchmark data

# REM: the row names are hard coded and could have changed
rowNames <- c("Time", "Benchmark", "VirtualMachine", "Platform", "ExtraArguments", "Cores", "Iterations", "None", "Criterion")

bench <- rbind(read.table("~/Projects/PhD/IBM/notes/scheduling-issue/osx.data.csv",    sep="\t", header=FALSE, col.names=rowNames),
               read.table("~/Projects/PhD/IBM/notes/scheduling-issue/ubuntu.data.csv", sep="\t", header=FALSE, col.names=rowNames),
               read.table("~/Projects/PhD/IBM/notes/scheduling-issue/tilera.data.csv", sep="\t", header=FALSE, col.names=rowNames))

# Some helpers
confInterval095Error <- function (samples) {
  if (length(sample) < 30)
    qnorm(0.975) * sd(samples) / sqrt(length(samples))
  else
    qt(0.975, df=length(samples)-1) * sd(samples) / sqrt(length(samples))
  }

# Separate Data for display

# TOOD: do the statistics on the data, get the mean, median, stddev, conf-interval for each
#       benchmark
norm_bench <- ddply(bench, ~Benchmark + VirtualMachine + Platform + ExtraArguments + None + Criterion,
         transform,
         Time.norm = Time / Time[Cores == min(Cores)])

# Prepare some readable/shorter names for the plots, hope we can map that somehow
bench_names = data.frame( "BenchmarkGameSuite.benchFasta.%(cores)s 1 15000"="Fasta",
                          "BenchmarkGameSuite.benchNBody.%(cores)s 1 4000" = "NBody", 
                          "BenchmarkGameSuite.benchFannkuchRedux.%(cores)s 1 7"="FannkuchRedux",
                          "BenchmarkGameSuite.benchBinaryTrees.%(cores)s 1 9" = "BinaryTrees", 
                          "SMarkLoops.benchIntLoop.%(cores)s 30"="Int Loop (1 proc per core)",
                          "SMarkCompiler.%(cores)s 50"="Compiler", 
                          "SMarkLoops.benchFloatLoop.%(cores)s 9"="Float Loop (1 proc per core)",
                          "SMarkLoops.benchFloatLoop.%(cores)s0 5"="Float Loop (10 procs per core)",
                          "SMarkLoops.benchIntLoop.%(cores)s0 5"="Int Loop (10 procs per core)",
                          "BenchmarkGameSuite.benchChameleons.None"="Chameleons")

stats <- ddply(norm_bench, ~Benchmark + VirtualMachine + Platform + ExtraArguments + Cores + None + Criterion,
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

# Get some basic infos for the plot
xrange <- range(stats$Cores)
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


