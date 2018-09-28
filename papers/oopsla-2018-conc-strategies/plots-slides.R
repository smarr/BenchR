if (sys.nframe() > 0) {
  script.dir <- dirname(sys.frame(1)$ofile)
  setwd(script.dir)
}

if (Sys.getenv("RSTUDIO") == "1") { setwd("/Users/smarr/Collab/Benoit-Daloze/concurrent_strategies_paper/data") }



library(extrafont)  ## using fonts requires to run ./setup.R
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(forcats)
library(RColorBrewer)

get_colors <- function(names) {
  sapply(names, get_color)
}

get_color <- function(name) {
  if (name == "Ruby") {
    # brewer.pal(9, "Set1")[1]
    # brewer.pal(6, "Paired")[2]
    brewer.pal(9, "Reds")[8]
  } else if (name == "Java") {
    # brewer.pal(9, "Set1")[2]
    # brewer.pal(6, "Dark2")[6]
    brewer.pal(9, "Blues")[6]
  } else if (name == "Fortran") {
    # brewer.pal(9, "Set1")[3]
    brewer.pal(6, "Paired")[3]
  } else if (name == "Concurrent Strategies" | name == "SharedFixedStorage" | name == "Sidekiq PDF Invoice") {
    brewer.pal(6, "Paired")[2]
  } else if (name == "LightweightLayoutLock") {
    brewer.pal(6, "Paired")[6]
  } else if (name == "LayoutLock") {
    brewer.pal(6, "Paired")[3]
  } else if (name == "StampedLock") {
    brewer.pal(6, "Paired")[4]
  } else if (name == "ReentrantLock" | name == "MRI") {
    # brewer.pal(6, "Paired")[5]
    brewer.pal(6, "Dark2")[6]
  } else {
    # Baseline: TruffleRuby, SharedDynamicStorage, VolatileFixedStorage, JRuby, Local, Sidekiq Sum
    brewer.pal(6, "Paired")[6]
  }
}

my_theme <- function(font_size = 9, legend.pos = c(1,0), legend.size = 8) {
  legend.dir = "vertical"
  if (identical(legend.pos, "bottom")) {
    legend.dir = "horizontal"
  }

  theme_bw() +
    theme(
      panel.grid.minor = element_line(size=0.2),
      panel.grid.major = element_line(size=0.2),
      legend.justification=legend.pos, legend.position=legend.pos,
      # no legend title, background, key
      legend.title = element_blank(),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.margin = margin(t=-0.05,b=0, l=0.15,r=0.2, unit="cm"),
      legend.direction = legend.dir,
      text = element_text(size=font_size, family = "Calibri"),
      axis.title.x = element_text(size = font_size),
      axis.title.y = element_text(size = font_size),
      axis.text.x = element_text(size = font_size, lineheight=0.7),
      axis.text.y = element_text(size = font_size),
      legend.text = element_text(size = legend.size),
      plot.margin = unit(c(0,0,0,0.1), "cm")
    )
}

no_y_label = theme(axis.title.y = element_blank())
small_facet_label = theme(strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm")))

load_raw_data <- function(filename) {
  d = read.csv(filename, header=FALSE, col.names=c("Bench", "Threads", "VM", "Iteration", "Value"), sep=";")
  n = length(d$Value)
  d$VM = factor(d$VM)
  d$VM = recode(d$VM, FastLayoutLock="LightweightLayoutLock",
                      FixedSize="SharedFixedStorage",
                      VolatileFixedSize="VolatileFixedStorage",
                      Sequential="Local")
  d = d %>% mutate(Orig=Value)
  d
}
  
load_data <- function(filename, ...) {
  d = load_raw_data(filename)
  transform_data(d, ...)
}

transform_data <- function(d, invert=FALSE, pypy=FALSE, stat=median, warmup=TRUE, npb=FALSE, norm=FALSE) {
  # Remove Synchronized
  d = subset(d, VM!="Synchronized")

  if (is.numeric(warmup)) {
    d = subset(d, d$Iteration>=warmup)
  } else if (warmup) {
    # Remove warmup
    d = subset(d, d$Iteration>=2)
  }

  if (invert) {
    base = stat(subset(d, Threads=="1" & VM=="Local")$Value)
    d$Value = d$Value / base
  }

  if (norm) {
    base = stat(subset(d, Threads=="1")$Value)
    d$Value = d$Value / base
  }

  if (pypy) {
    base = median(subset(d, Threads=="1" & VM=="Local")$Value)
    d$Value = base / d$Value
  }

  if (npb) {
    d = d %>% group_by(Bench, VM) %>% mutate(
      singleThreadPerf = median(subset(Value, Threads==1)),
      Value = singleThreadPerf / Value
    )
  }

  d = d %>% group_by(Bench, Threads, VM) %>% mutate(
    Median = stat(Value),
    Min = min(Value),
    Max = max(Value))
  
  d = d %>% group_by(Bench, VM) %>% mutate(
    Ideal = Threads * max(subset(Value, Threads=="1")))

  # Sort legend by best first
  d$VM = fct_reorder(d$VM, d$Median, fun=max, .desc = TRUE)
  if ("StampedLock" %in% levels(d$VM)) {
    d$VM = fct_relevel(d$VM, "LightweightLayoutLock", "LayoutLock", "StampedLock", "ReentrantLock", after=Inf)
  }
  # Local last
  if ("Local" %in% levels(d$VM)) {
    d$VM = fct_relevel(d$VM, "Local", after=Inf)
  }

  data.frame(d)
}

gen_plot <- function(data_file, invert=FALSE, pypy=FALSE, stat=median, ...) {
  full = load_data(data_file, invert=invert, pypy=pypy, stat=stat)
  render_plot(full, ...)
}

render_plot <- function(full, output_pdf, scalability_lines=TRUE,
                     legend.pos=c(0,1), ops=1, multiplier=1,
                     error_bars=FALSE, legend.size = 8, sequential.line = FALSE, legend.nrow = FALSE,
                     ylabel = "Throughput", y_breaks = waiver(), font_size=9,
                     facets=FALSE, manual_points=FALSE) {
  plot = ggplot(data = full, aes(x=Threads, y=Median, group=VM, color=VM))
  
  if (sequential.line) {
    seq <- subset(full, VM=="Local")$Median
    plot = plot + geom_hline(yintercept = seq, linetype = "dashed", colour = get_color("Local"))
  }
  
  xbreaks = unique(full$Threads)
  xbreaks = subset(xbreaks, xbreaks!=22)
  
  if (!manual_points) {
    plot = plot + geom_point(size=1.5, aes(shape=VM)) +
                  scale_shape_manual(values=c(16,17,15,4,1))
  }
  plot = plot +
    geom_line(size=0.5) +
    my_theme(legend.pos=legend.pos, font_size=font_size, legend.size = legend.size) +
    xlab("Threads") + ylab(ylabel) +
    scale_x_continuous(breaks = xbreaks, minor_breaks = NULL) +
    scale_y_continuous(breaks=y_breaks, labels=function(x)x*ops/multiplier)
  
  if (facets) {
    # plot = plot + coord_cartesian(xlim = c(2, max(full$Threads) - 1))
  } else {
    plot = plot + coord_cartesian(xlim = c(0, max(full$Threads) + 2), ylim = c(0, max(full$Value)*1.05), expand=FALSE)
  }

  # plot = plot + geom_line(data = subset(full, VM == "Local"), size=0.4, color="black", linetype=3)
  # plot = plot + scale_colour_brewer(palette = "Paired")
  plot = plot + scale_colour_manual(values = get_colors(levels(droplevels(full)$VM)))

  if (!identical(legend.nrow, FALSE)) {
    plot = plot + guides(shape = guide_legend(nrow = legend.nrow,
                                              keyheight = 0.5))
    plot = plot + guides(colour = guide_legend(nrow = legend.nrow,
                                              keyheight = 0.5))
  }
  
  if (error_bars) {
    plot = plot + geom_errorbar(aes(ymin=Min, ymax=Max))
  }

  if (scalability_lines) {
    subdata = subset(full, VM=="SharedFixedStorage" | VM=="LightweightLayoutLock" | VM=="Local" | VM=="Sidekiq Sum" | VM=="TruffleRuby")
    plot = plot + geom_line(data = subdata, aes(x=Threads, y=Ideal), color="black", size=0.2)
  }
  
  if (facets) {
    plot = plot + facet_wrap(~ Bench, scales = "free_y", ncol = 4)
  }
  plot
}

# r = load_data("x62_conc_reads_volatile1.csv", stat=median)
# rw10 = load_data("x62_conc_rw10_90_volatile1.csv", stat=max)
# rw50 = load_data("x62_conc_rw50_50_volatile1.csv", stat=max)
# w = load_data("x62_conc_writes_volatile1.csv", stat=max)

r = load_data("x62_conc_reads_volatile2.csv", stat=median)
rw10 = load_data("x62_conc_rw10_90_volatile2.csv", stat=max)
rw50 = load_data("x62_conc_rw50_50_volatile2.csv", stat=max)
w = load_data("x62_conc_writes_volatile2.csv", stat=max)

array_rw = rbind(r, rw10, rw50)
array_rw$Bench = recode(array_rw$Bench,
                    bench_array_conc_reads_ops.rb="100% reads",
                    bench_array_conc_write_reads_ops_10_90.rb="10% writes, 90% reads",
                    bench_array_conc_write_reads_ops_50_50.rb="50% writes, 50% reads",
                    bench_array_conc_write_ops.rb="100% writes")

render_plot(array_rw, ops=100*1000, multiplier=1e9, facets=TRUE, legend.pos="bottom", legend.nrow=1,
            ylabel="Billion array accesses per sec.", manual_points=TRUE) +
  small_facet_label +
  geom_point(aes(shape=VM, size=VM)) +
  scale_shape_manual(values=c(16,17,15,3,1,4)) +
  scale_size_manual(values=c(1.5,1.5,1.5,1.5,2.0,1.3)) +
  theme(legend.margin = margin(t=-0.25, b=0, l=0.15, r=0.2, unit="cm"))
ggsave("conc_reads_writes.pdf", width = 7.00, height = 1.9, device = cairo_pdf)

gen_plot("x62_conc_appends3.csv", scalability_lines=FALSE, legend.pos=c(1, 0.1), legend.nrow=4, ops=1000, multiplier=1e6,
         ylabel="Million appends per second") +
  theme(plot.margin = unit(c(0.1,0,0,0.1), "cm")) # Some margin for the y axis label
ggsave("conc_appends.pdf", width = 2.24, height = 1.7, device = cairo_pdf)

safepoints = load_data("x62_safepoints2.csv")
safepoints$VM = recode(safepoints$VM, LightweightLayoutLock="SharedDynamicStorage")
render_plot(safepoints, scalability_lines=FALSE, ops=1000, multiplier=1e6, error_bars=TRUE, legend.pos=c(1,0),
         ylabel="Million reads per second") + theme(legend.key.size = unit(0.75, 'lines'))
ggsave("safepoints.pdf", width = 2.24, height = 1.7, device = cairo_pdf)

hash_ops = load_data("x62_hash_ops_1.csv", invert=TRUE)
render_plot(hash_ops, scalability_lines=FALSE, sequential.line = TRUE, ylabel="Scalability of Hash ops.", legend.pos=c(-0.1,1.05), manual_points=TRUE) +
  geom_point(aes(shape=VM, size=VM)) + scale_shape_manual(values=c(16,17,15,4,5)) + scale_size_manual(values=c(1.5,1.5,1.5,1.5,2.5)) +
  theme(plot.margin = margin(t=0,b=0, l=0.05,r=0, unit="cm"), # Some margin for the y axis label
        legend.key.size = unit(0.75, 'lines'))
ggsave("hash_ops_80_10_10.pdf", width = 2.24, height = 1.7, device = cairo_pdf)

gen_plot("group_by1.csv", scalability_lines=FALSE, invert=TRUE,
         sequential.line = TRUE, ylabel = "Scalability relative to Local", y_breaks = seq(0, 44, 1),
         legend.pos=c(0,1.0), manual_points=TRUE) +
  geom_point(aes(shape=VM, size=VM)) + scale_shape_manual(values=c(16,17,15,4,5)) + scale_size_manual(values=c(1.5,1.5,1.5,1.5,2.0)) +
  theme(plot.margin = margin(t=0.15,b=0,l=0.05,r=0, unit="cm"), # Some margin for the y axis label
        legend.key.size = unit(0.75, 'lines'))
ggsave("histogram_fixed_workload_40M_100K.pdf", width = 3.33, height = 1.65, device = cairo_pdf)

mandel1shared = load_raw_data("x62_pypy_mandelbrot1.csv")
mandel1local = subset(load_raw_data("x62_pypy_mandelbrot_both2.csv"), VM=="Local")
mandel = rbind(mandel1shared, mandel1local)
mandel = transform_data(mandel, pypy = TRUE)
mandel$VM = recode(mandel$VM, Local="TruffleRuby",
                              SharedFixedStorage="Concurrent Strategies")

legendy = 1.03

render_plot(mandel, error_bars=TRUE, ylabel = "Scalability Factor", y_breaks = seq(0, 44, 4), legend.pos=c(0,legendy)) +
  theme(legend.key.size = unit(0.75, 'lines'))
# gen_plot("x62_pypy_mandelbrot_both2.csv", "pypy_mandelbrot.pdf", pypy=TRUE, error_bars=TRUE)
ggsave("pypy_mandelbrot.pdf", width = 2.24, height = 1.5, device = cairo_pdf)

# gen_plot("x62_pypy_raytrace1.csv", "pypy_raytrace.pdf", pypy=TRUE, error_bars=TRUE)
# raytrace = load_data("x62_pypy_raytrace_both2.csv", pypy=TRUE)
# render_plot(raytrace, error_bars=TRUE, ylabel = "Scalability", y_breaks = seq(0, 44, 2))
# ggsave("pypy_raytrace.pdf", width = 2.24, height = 1.5, device = cairo_pdf)

truffleruby = load_data("x62_threaded_reverse_10k.csv", norm=TRUE)
truffleruby$VM = recode(truffleruby$VM, SharedFixedStorage="Concurrent Strategies")
jruby = load_data("x62_threaded_reverse_jruby_10k.csv", norm=TRUE)
threaded_reverse = rbind(truffleruby, jruby)
render_plot(threaded_reverse, error_bars=TRUE, ylabel = "Scalability", scalability_lines = FALSE, legend.pos=c(0,legendy)) + no_y_label +
  theme(legend.key.size = unit(0.75, 'lines'))
  # theme(plot.margin = unit(c(0.2,0,0,0), "cm")) # Some margin for the y axis label
ggsave("threaded_reverse.pdf", width = 2.24, height = 1.5, device = cairo_pdf)

# NPB
java = load_data("npb_java_all3.csv", warmup=TRUE, npb=TRUE)
fortran = load_data("npb_fortran3.csv", warmup=FALSE, npb=TRUE)
ruby = load_data("npb_ruby_full1.csv", warmup=TRUE, npb=TRUE)

npb = rbind(java, fortran, ruby)
npb$VM = fct_relevel(npb$VM, "Fortran", "Java", "Ruby")

render_plot(npb, "npb_scalability.pdf", facets=TRUE, error_bars=TRUE, ylabel="Scalability relative to 1 thread performance",
            legend.pos=c(0,1.0)) + small_facet_label +
  expand_limits(y=0) +
  theme(legend.key.size = unit(0.75, 'lines'))
ggsave("npb_scalability.pdf", width = 7.00, height = 2.9, device = cairo_pdf)

# Realistic Ruby benchmarks

sidekiq_sum = load_data("x62_mini_sidekiq_16cores1.csv", warmup=TRUE, norm=TRUE)
sidekiq_sum$VM = recode(sidekiq_sum$VM, LightweightLayoutLock="Sidekiq Sum")

sidekiq_prawn = load_data("sidekiq_prawn_invoice8_10k_no_cache_sync.csv", warmup=TRUE, norm=TRUE)
sidekiq_prawn$VM = recode(sidekiq_prawn$VM, SharedFixedStorage="Sidekiq PDF Invoice")
sidekiq_prawn_count = 10000

full = rbind(sidekiq_sum, sidekiq_prawn)
render_plot(full, error_bars=FALSE, scalability_lines = TRUE, ylabel = "Scalability", legend.pos=c(0,legendy)) + no_y_label +
  theme(plot.margin = unit(c(0.1,0,0,0), "cm")) + # Some margin for the top y label
  theme(legend.key.size = unit(0.75, 'lines'))
ggsave("sidekiq.pdf", width = 2.24, height = 1.5, device = cairo_pdf)


mri = load_data("webrick_mri7_nopool.csv", warmup=TRUE)
jruby = load_data("webrick_jruby7_nopool.csv", warmup=TRUE)
tr = load_data("webrick_truffleruby10_nopool_nowaitcreatethread_internalpool_xms64.csv", warmup=20)
webrick = rbind(mri,jruby,tr)
webrick$VM = recode(webrick$VM, SharedFixedStorage="Concurrent Strategies")
webrick$VM = fct_relevel(webrick$VM, "Concurrent Strategies", "JRuby", "MRI")
webrick_mri1 = round(unique(subset(webrick, VM=="MRI" & Threads==1)$Median), -1)

render_plot(webrick, error_bars=FALSE, scalability_lines = FALSE, ylabel = "Requests per second", legend.pos=c(-0.02,1.08)) + #, y_breaks = c(webrick_mri1, seq(2000, 12000, 2000))) +
  scale_y_continuous(breaks = c(webrick_mri1, seq(2000, 12000, 2000)),  minor_breaks = NULL) +
  theme(legend.key.size = unit(0.75, 'lines'))
ggsave("webrick.pdf", width = 3.33, height = 1.4, device = cairo_pdf)

### Are-We-Fast-Yet ###

load_awfy <- function(file, row_names) {
  row_names = c("TimeStamp", "Value", "Unit", "Criterion", "Benchmark",
                "VM", "Suite", "Extra", "Warmup", "Cores", "InputSize", "Var")
  d = read.table(file, sep="\t", header=FALSE, col.names=row_names, fill=TRUE)
  d$Benchmark = factor(d$Benchmark)
  # Add Iteration
  d = d %>% group_by(Benchmark, VM, Var, Extra, Cores, Suite) %>% mutate(
    Iteration = seq_len(length(Value)))

  # Remove warmup
  d = subset(d, d$Iteration>=200)

  # Stats
  d = d %>% group_by(Benchmark, VM) %>% mutate(
    Median = median(Value),
    Min = min(Value),
    Max = max(Value))

  # Normalize results with the baseline configuration
  d = d %>% group_by(Benchmark) %>% mutate(
    Norm = Value / median(subset(Value, VM=="NoSharing")))

  # Rename
  d$VM = recode(d$VM, NoSharing="TruffleRuby",
                      Sharing="Concurrent Strategies")

  data.frame(d)
}

small = load_awfy("awfy_small1.csv")
big = load_awfy("awfy_big1.csv")
full = rbind(small, big)

awfy_breaks <- levels(droplevels(full)$VM)
awfy_colors <- get_colors(awfy_breaks)

ggplot(data = full, aes(x=Benchmark, y=Norm, color=VM)) +
  geom_boxplot(outlier.size=0.3,lwd=0.5) +
  my_theme(legend.pos="bottom") +
  ylab("Runtime factor") +
  xlab("") +
  scale_colour_manual(values = awfy_colors) +
  coord_cartesian(ylim = c(0.9, 1.4), expand=FALSE, xlim = c(0.5,length(levels(factor(full$Benchmark)))+0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        plot.margin = unit(c(0.15,0,0,0.1), "cm"),
        legend.margin=margin(t = -0.5, unit='cm')) # Some margin for the top y label

ggsave("slides-awfy.pdf", width = 6, height = 3, device = cairo_pdf)

# STATS
unlink("data.tex")

write_stat = function(name, value) {
  write(paste0("\\def\\stats", name, "{", value, "}"), "data.tex", append = TRUE)
}

fixed_speedups = array_rw %>%
  group_by(Bench, VM, Threads) %>% summarise(
    Value = unique(Median)
  ) %>% group_by(Bench, Threads) %>% mutate(
    MinFixedSpeedupLLL = subset(Value, VM=="SharedFixedStorage") / max(subset(Value, VM=="LightweightLayoutLock")),
    MaxFixedSpeedupLLL = subset(Value, VM=="SharedFixedStorage") / min(subset(Value, VM=="LightweightLayoutLock")),
    MinFixedSpeedupVol = subset(Value, VM=="SharedFixedStorage") / max(subset(Value, VM=="VolatileFixedStorage")),
    MaxFixedSpeedupVol = subset(Value, VM=="SharedFixedStorage") / min(subset(Value, VM=="VolatileFixedStorage")),
    MaxFixedSpeedup = subset(Value, VM=="SharedFixedStorage") / min(subset(Value, VM!="SharedFixedStorage"))
  ) %>% group_by() %>% summarize(
    MinFixedSpeedupLLL = min(MinFixedSpeedupLLL),
    MaxFixedSpeedupLLL = max(MaxFixedSpeedupLLL),
    MinFixedSpeedupVol = min(MinFixedSpeedupVol),
    MaxFixedSpeedupVol = max(MaxFixedSpeedupVol),
    MaxFixedSpeedup = max(MaxFixedSpeedup))

write_stat("MinArrayFixedSpeedupLLL", round(fixed_speedups$MinFixedSpeedupLLL, 1))
write_stat("MaxArrayFixedSpeedupLLL", round(fixed_speedups$MaxFixedSpeedupLLL, 1))
write_stat("MinArrayFixedSpeedupVol", round(fixed_speedups$MinFixedSpeedupVol, 0))
write_stat("MaxArrayFixedSpeedupVol", round(fixed_speedups$MaxFixedSpeedupVol, 1))
write_stat("MaxArrayFixedSpeedup", round(fixed_speedups$MaxFixedSpeedup, 1))

npb_speedups = subset(npb, Threads==1) %>% group_by(Bench, VM, Threads) %>% summarise(
  Value = median(Orig)
) %>% group_by(Bench, Threads) %>% mutate(
  RubyOverJava = subset(Value, VM=="Ruby") / max(subset(Value, VM=="Java")),
  JavaOverFortran = subset(Value, VM=="Java") / max(subset(Value, VM=="Fortran")),
  RubyOverFortran = subset(Value, VM=="Ruby") / max(subset(Value, VM=="Fortran"))
) %>% group_by(Bench) %>% summarize(
  MinRubyOverJava = min(RubyOverJava),
  MaxRubyOverJava = max(RubyOverJava),
  MedianRubyOverJava = median(RubyOverJava),

  MinJavaOverFortran = min(JavaOverFortran),
  MaxJavaOverFortran = max(JavaOverFortran),
  MedianJavaOverFortran = median(JavaOverFortran),

  MinRubyOverFortran = min(RubyOverFortran),
  MaxRubyOverFortran = max(RubyOverFortran),
  MedianRubyOverFortran = median(RubyOverFortran)
)

geomean <- function(x) { exp(mean(log(x))) }

write_stat("NPBGeomeanRubyJava", round(geomean(npb_speedups$MedianRubyOverJava), 1))
write_stat("NPBMinRubyJava", round(min(npb_speedups$MedianRubyOverJava), 1))
write_stat("NPBMaxRubyJava", round(max(npb_speedups$MedianRubyOverJava), 1))

write_stat("NPBGeomeanJavaFortran", round(geomean(npb_speedups$MedianJavaOverFortran), 1))
write_stat("NPBMinJavaFortran", round(min(npb_speedups$MedianJavaOverFortran), 1))
write_stat("NPBMaxJavaFortran", round(max(npb_speedups$MedianJavaOverFortran), 1))

write_stat("NPBGeomeanRubyFortran", round(geomean(npb_speedups$MedianRubyOverFortran), 1))
write_stat("NPBMinRubyFortran", round(min(npb_speedups$MedianRubyOverFortran), 1))
write_stat("NPBMaxRubyFortran", round(max(npb_speedups$MedianRubyOverFortran), 1))

# local1 = subset(raytrace, VM=="Local" & Threads==1)$Median
# fixed4 = subset(raytrace, VM=="SharedFixedStorage" & Threads==4)$Median
# write_stat("RaytraceScalingFourth", unique(fixed4/local1))

# local1 = subset(raytrace, VM=="Local" & Threads==1)$Median
# fixed8 = subset(raytrace, VM=="SharedFixedStorage" & Threads==8)$Median
# write_stat("RaytraceScalingEigth", unique(fixed8/local1))

local1 = subset(mandel, VM=="TruffleRuby" & Threads==1)$Median
fixed = subset(mandel, VM=="Concurrent Strategies" & Threads==8)$Median
write_stat("MandelbrotScalingEigth", round(unique(fixed/local1)))
fixed = subset(mandel, VM=="Concurrent Strategies" & Threads==22)$Median
write_stat("MandelbrotScalingTwentyTwo", round(unique(fixed/local1)))
fixed = subset(mandel, VM=="Concurrent Strategies" & Threads==40)$Median
write_stat("MandelbrotScalingFourty", round(unique(fixed/local1)))

local1 = subset(hash_ops, VM=="Local" & Threads==1)$Median
ll = subset(hash_ops, VM=="LayoutLock" & Threads==1)$Median
lll = subset(hash_ops, VM=="LightweightLayoutLock" & Threads==1)$Median
lll44 = subset(hash_ops, VM=="LightweightLayoutLock" & Threads==44)$Median
write_stat("HashOpsOverheadLL", round((unique(local1/ll)-1)*100))
write_stat("HashOpsOverheadLLL", round((unique(local1/lll)-1)*100))
write_stat("HashOpsMaxSpeedup", round(unique(lll44/local1)))
lll44ops = median(subset(hash_ops, VM=="LightweightLayoutLock" & Threads==44)$Orig)
write_stat("HashOpsMaxOpsLLL", round(lll44ops))

# d = subset(npb, Threads==1) %>% group_by(Bench, VM) %>% summarize(MinTime=min(Orig), MaxTime=max(Orig))
d = subset(npb, Threads==1) %>% summarize(MinTime=min(Orig), MaxTime=max(Orig))
write_stat("NPBMinSingleThreadTime", round(d$MinTime))
write_stat("NPBMaxSingleThreadTime", round(d$MaxTime/60))

speedup = median(subset(sidekiq_prawn, Threads==16)$Value)
write_stat("PrawnSpeedupSixteen", round(speedup))
d = median(subset(sidekiq_prawn, Threads==16)$Orig)
pdfs_per_sec = sidekiq_prawn_count/d
write_stat("PrawnPDFsPerSec", round(pdfs_per_sec))

jruby1 = median(subset(threaded_reverse, Threads==1 & VM=="JRuby")$Orig)
truby1 = median(subset(threaded_reverse, Threads==1 & VM=="Concurrent Strategies")$Orig)
speedup1 = truby1/jruby1
write_stat("ThreadedReverseSpeedupOne", round(speedup1))

jruby44 = median(subset(threaded_reverse, Threads==44 & VM=="JRuby")$Orig)
truby44 = median(subset(threaded_reverse, Threads==44 & VM=="Concurrent Strategies")$Orig)
speedup44 = truby44/jruby44
write_stat("ThreadedReverseSpeedupAll", round(speedup44))

truby44speedup = median(subset(threaded_reverse, Threads==44 & VM=="Concurrent Strategies")$Value)
write_stat("ThreadedReverseMaxSpeedup", round(truby44speedup))

webrickMRISpeedup = unique(subset(webrick, Threads==8 & VM=="Concurrent Strategies")$Median) / unique(subset(webrick, Threads==8 & VM=="MRI")$Median)
webrickJRubySpeedup = unique(subset(webrick, Threads==8 & VM=="Concurrent Strategies")$Median) / unique(subset(webrick, Threads==8 & VM=="JRuby")$Median)
write_stat("WEBrickMRISpeedup", round(webrickMRISpeedup, 1))
write_stat("WEBrickJRubySpeedup", round(webrickJRubySpeedup, 1))
