### Generate the Weak-Scaling Graphs for Intel + TILEPro64
source("~/Projects/PhD/IBM/notes/scheduling-issue/helper.R")

bench <- load_data()


## Goal
# Graph 1 - Intel showing weak scalability
# Graph 2 - Tilera, showing weak scalability (<=16 cores)
# Graph 3 - Tilera, showing weak scalability (all cores)
## Runtime normalized to 1-core value


## Normalize data
norm_bench <- ddply(bench, ~ Benchmark + VirtualMachine + Platform + ExtraArguments + None + Criterion,
                    transform,
                    Time.norm = Time / Time[Cores == 1])   ## Normalize to 1-core runtime per benchmark

## Add all the necessary statistic values
stats <- ddply(norm_bench, ~ Benchmark + VirtualMachine + Platform + ExtraArguments + Cores + None + Criterion,
         summarise,
         #Time.mean=mean(Time),  # will rely on the normalized values only
         #Time.stddev=sd(Time),
         #Time.median=median(Time),
         #Time.mean095Error=confInterval095Error(Time),
         #Time.cnfIntHigh = mean(Time) + (confInterval095Error(Time)),
         #Time.cnfIntLow = mean(Time) - (confInterval095Error(Time)),
         Time.mean.norm=mean(Time.norm),
         Time.stddev.norm=sd(Time.norm),
         Time.median.norm=median(Time.norm),
         Time.mean095Error.norm=confInterval095Error(Time.norm),
         Time.cnfIntHigh.norm = mean(Time.norm) + (confInterval095Error(Time.norm)),
         Time.cnfIntLow.norm = mean(Time.norm) - (confInterval095Error(Time.norm)),
         BId = interaction(Benchmark[1], ExtraArguments[1], drop=TRUE))

# Graph 1 - Intel showing weak scalability

# filter out intel data
intel_data <- drop_unused_factors(subset(stats, VirtualMachine == "rvm-intel-8u"))
intel_scale <- custom_scale(intel_data$BId)


p <- ggplot(intel_data, aes(Cores, Time.mean.norm, group = BId, colour = BId))
p <- p + geom_line(size = 1)
p <- p + geom_errorbar(aes(ymin=Time.cnfIntLow.norm, ymax = Time.cnfIntHigh.norm))
p <- p + scale_shape_manual("Benchmarks", values=intel_scale)
p <- p + scale_colour_manual("Benchmarks",values=intel_scale)
p <- p + geom_point(aes(shape=BId, size=4))
p <- p + xlab("Cores") + ylab("Runtime normalized to 1-core")
p <- p + opts(title="Intel 8cores, 16 hyperthreads")
#p <- p + opts(legend.position="bottom")
p <- p + scale_x_continuous(breaks=1:16)
p
ggsave(filename="~/Projects/PhD/IBM/notes/scheduling-issue/intel.pdf", plot=p)


# Graph 2.. - Tilera, showing weak scalability (<=16 cores, ...)
tilera_data <- drop_unused_factors(subset(stats, VirtualMachine == "rvm-tilera"))
tilera_scale <- custom_scale(tilera_data$BId)

p <- ggplot(tilera_data, aes(Cores, Time.mean.norm, group = BId, colour = BId))
p <- p + geom_line(size = 1)
p <- p + geom_errorbar(aes(ymin=Time.cnfIntLow.norm, ymax = Time.cnfIntHigh.norm))
p <- p + scale_shape_manual("Benchmarks", values=tilera_scale)
p <- p + scale_colour_manual("Benchmarks",values=tilera_scale)
p <- p + geom_point(aes(shape=BId, size=4))
p <- p + xlab("Cores") + ylab("Runtime normalized to 1-core")
p <- p + opts(title="Tilera 16 cores")
#p <- p + opts(legend.position="bottom")
p <- p + scale_x_continuous(breaks=1:16)
p <- p + scale_x_continuous(limits=c(1, 16))
p <- p + scale_y_continuous(limits=c(1, 5))
p
ggsave(filename="~/Projects/PhD/IBM/notes/scheduling-issue/tilera-16cores.pdf", plot=p)

p <- ggplot(tilera_data, aes(Cores, Time.mean.norm, group = BId, colour = BId))
p <- p + geom_line(size = 1)
p <- p + geom_errorbar(aes(ymin=Time.cnfIntLow.norm, ymax = Time.cnfIntHigh.norm))
p <- p + scale_shape_manual("Benchmarks", values=tilera_scale)
p <- p + scale_colour_manual("Benchmarks",values=tilera_scale)
p <- p + geom_point(aes(shape=BId, size=4))
p <- p + xlab("Cores") + ylab("Runtime normalized to 1-core")
p <- p + opts(title="Tilera 32 cores")
#p <- p + opts(legend.position="bottom")
p <- p + scale_x_continuous(breaks=1:32)
p <- p + scale_x_continuous(limits=c(1, 32))
p <- p + scale_y_continuous(limits=c(1, 15))
p
ggsave(filename="~/Projects/PhD/IBM/notes/scheduling-issue/tilera-32cores.pdf", plot=p)

p <- ggplot(tilera_data, aes(Cores, Time.mean.norm, group = BId, colour = BId))
p <- p + geom_line(size = 1)
p <- p + geom_errorbar(aes(ymin=Time.cnfIntLow.norm, ymax = Time.cnfIntHigh.norm))
p <- p + scale_shape_manual("Benchmarks", values=tilera_scale)
p <- p + scale_colour_manual("Benchmarks",values=tilera_scale)
p <- p + geom_point(aes(shape=BId, size=4))
p <- p + xlab("Cores") + ylab("Runtime normalized to 1-core")
p <- p + opts(title="Tilera 59 cores")
#p <- p + opts(legend.position="bottom")
p <- p + scale_x_continuous(breaks=1:59)
p <- p + scale_x_continuous(limits=c(1, 59))
p <- p + scale_y_continuous(limits=c(1, 30))
p
ggsave(filename="~/Projects/PhD/IBM/notes/scheduling-issue/tilera-59cores.pdf", plot=p)

p <- ggplot(tilera_data, aes(Cores, Time.mean.norm, group = BId, colour = BId))
p <- p + geom_line(size = 1)
p <- p + geom_errorbar(aes(ymin=Time.cnfIntLow.norm, ymax = Time.cnfIntHigh.norm))
p <- p + scale_shape_manual("Benchmarks", values=tilera_scale)
p <- p + scale_colour_manual("Benchmarks",values=tilera_scale)
p <- p + geom_point(aes(shape=BId, size=4))
p <- p + xlab("Cores") + ylab("Runtime normalized to 1-core")
p <- p + opts(title="Tilera 59 cores")
#p <- p + opts(legend.position="bottom")
p
ggsave(filename="~/Projects/PhD/IBM/notes/scheduling-issue/tilera-complete.pdf", plot=p)


