## RoarVM vs. Squeak 4.4.7.2357 vs. Cog r2390
source("~/Projects/PhD/IBM/notes/scheduling-issue/helper.R")

bench <- load_data()

# Graph for the benchmarks plotting Runtime vs. Cores, and for one benchmark the three VMs


## Add all the necessary statistic values
stats <- ddply(bench, ~ Benchmark + VirtualMachine + Platform + ExtraArguments + Cores + None + Criterion,
         summarise,
         Time.mean=mean(Time),
         Time.stddev=sd(Time),
         Time.median=median(Time),
         Time.mean095Error=confInterval095Error(Time),
         Time.cnfIntHigh = mean(Time) + (confInterval095Error(Time)),
         Time.cnfIntLow = mean(Time) - (confInterval095Error(Time)),
         BId = interaction(Benchmark[1], ExtraArguments[1], drop=TRUE))

# helper function for creating the plot, on per benchmark

# Looking at the compiler benchmark
compiler_bench <- drop_unused_factors(subset(stats, (Benchmark == "SMarkCompiler") & (VirtualMachine == "rvm-intel-8u" | VirtualMachine == "cog-vm" | VirtualMachine == "squeak-vm")))

p <- ggplot(compiler_bench, aes(Cores, Time.mean, group = VirtualMachine, colour = VirtualMachine))
p <- p + geom_line(size = 1)
p <- p + geom_errorbar(aes(ymin=Time.cnfIntLow, ymax = Time.cnfIntHigh))
p <- p + geom_point(aes(shape=VirtualMachine, size=4))
p <- p + xlab("Cores") + ylab("Runtime")
p <- p + opts(title="Compiler Benchmark for Weak-Scaling #Work==#Cores")
p <- p + scale_x_continuous(breaks=1:16)
p
ggsave(filename="~/Projects/PhD/IBM/notes/scheduling-issue/compiler-cog-roar-squeak.pdf", plot=p)



## versiob with facet_grid
compiler_bench <- drop_unused_factors(subset(stats, 
              (VirtualMachine == "rvm-intel-8u" | VirtualMachine == "cog-vm" | VirtualMachine == "squeak-vm")
              & !((Benchmark == "SMarkLoops.benchFloatLoop") & (ExtraArguments == "%(cores)s 9"))
              & !((Benchmark == "SMarkLoops.benchIntLoop") & (ExtraArguments == "%(cores)s 30")) ))

p <- ggplot(compiler_bench, aes(Cores, Time.mean, group = VirtualMachine, colour = VirtualMachine))
p <- p + geom_line(size = 1)
p <- p + geom_errorbar(aes(ymin=Time.cnfIntLow, ymax = Time.cnfIntHigh))
p <- p + geom_point(aes(shape=VirtualMachine, size=4))
p <- p + xlab("Cores") + ylab("Runtime")
p <- p + opts(title="Compiler Benchmark for Weak-Scaling #Work==#Cores")
p <- p + scale_y_continuous(limits=c(0,4000))
p <- p + facet_grid(. ~ Benchmark, scales = "free", space = "free")
p
ggsave(filename="~/Projects/PhD/IBM/notes/scheduling-issue/all-cog-roar-squeak.pdf", plot=p)

