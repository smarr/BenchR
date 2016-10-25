source("libraries.R")

actors_data <- function() {
  full = rbind(load_savina_data_file("../data/final/savina_scala_mul1207.csv.bz2", "Scala Akka"),
               load_data_file("../data/final/savina_all1207.csv.bz2"))

  # Fix naming
  full$VM = revalue(full$VM, c(
    "JRubyTruffleLocal" = "Unsafe",
    "JRubyTruffleShared" = "Safe",
    "JRubySharedSimple" = "No Deep Sharing"
  ))

  full$VM = factor(full$VM, levels = list("Scala Akka", "Unsafe", "Safe", "No Deep Sharing"))

  full$Benchmark = revalue(full$Benchmark, c(
    "SavinaTrapezoidal" = "Trapezoidal",
    "SavinaRadixSort" = "RadixSort",
    "SavinaApsp" = "APSP"
  ))
  full
}

actors_peak <- function(full) {
  # Iteration plots
  #qplot(y = Value, x = Iteration, data = subset(full, Benchmark=="RadixSort"), color = VM) + coord_cartesian(ylim = c(0,500))
  #qplot(y = Value, x = Iteration, data = subset(full, Benchmark=="APSP"), color = VM) + coord_cartesian(ylim = c(0,300))
  #qplot(y = Value, x = Iteration, data = subset(full, Benchmark=="Trapezoidal"), color = VM) + coord_cartesian(ylim = c(0,300))

  # Remove warmup
  peak <- subset(full, Iteration >= 600)
}

sequential_data <- function() {
  full = load_data_file("../data/final/single_all1207.csv.bz2")

  # Fix naming
  full$Benchmark = revalue(full$Benchmark, c("Json" = "JSON"))

  full$VM = revalue(full$VM, c(
    "JRubyTruffleLocal" = "Unsafe",
    "JRubyTruffleShared" = "Safe",
    "JRubyTruffleAll" = "All Shared"
  ))

  full$VM <- factor(full$VM, levels = list("Unsafe", "Safe", "All Shared"))

  # Remove Mandelbrot
  full = subset(full, Benchmark!="Mandelbrot")
  full
}

sequential_peak <- function(full) {
  # Remove warmup
  peak <- subset(full, Iteration >= 300 & Iteration <= 799)
}
