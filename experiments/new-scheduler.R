### Generate the Weak-Scaling Graphs for Intel + TILEPro64
source("~/Projects/PhD/IBM/notes/scheduling-issue/helper.R")

bench <- load_data_file("~/Projects/PhD/IBM/notes/scheduling-issue/new-scheduler.data.csv")
file_prefix_for_generated_pdfs <- "~/Projects/PhD/IBM/notes/scheduling-issue/new-scheduler_"


stats <- ddply(bench, ~ Benchmark + VirtualMachine + Platform + ExtraArguments + Cores + None + Criterion,
         summarise,
         Time.mean=mean(Time),  # will rely on the normalized values only
         Time.stddev=sd(Time),
         Time.median=median(Time),
         Time.mean095Error=confInterval095Error(Time),
         Time.cnfIntHigh = mean(Time) + (confInterval095Error(Time)),
         Time.cnfIntLow = mean(Time) - (confInterval095Error(Time)),
         BId = interaction(Benchmark[1], ExtraArguments[1], drop=TRUE))

create_plot <- function(data, benchId, folder) {
  bench_data <- drop_unused_factors(subset(data, BId == benchId))
  p <- ggplot(bench_data, aes(Cores, Time.mean, group = VirtualMachine, colour = VirtualMachine))
  p <- p + geom_line(size = 1)
  p <- p + geom_errorbar(aes(ymin=Time.cnfIntLow, ymax = Time.cnfIntHigh))
  p <- p + geom_point(aes(shape=VirtualMachine, size=4))
  p <- p + xlab("Cores") + ylab("Runtime in ms")
  p <- p + opts(title=paste("Intel - 8 cores (16 hyperthreads): ", benchId, sep=""))
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
  p <- create_plot(stats, benchId, file_prefix_for_generated_pdfs)
  p
}

