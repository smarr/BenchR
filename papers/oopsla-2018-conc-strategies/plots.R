if (sys.nframe() > 0) {
  script.dir <- dirname(sys.frame(1)$ofile)
  setwd(script.dir)
}

library(extrafont)  ## using fonts requires to run ./setup.R
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(forcats)
library(RColorBrewer)
library(grid)

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
    # brewer.pal(9, "Blues")[6]
    brewer.pal(6, "Paired")[4]
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
  } else if (name == "JRuby") {
    brewer.pal(6, "Paired")[3]
  } else if (name == "VolatileFixedStorage") {
    brewer.pal(12, "Paired")[9]
  } else {
    # Baseline: TruffleRuby, SharedDynamicStorage, Local, Sidekiq Sum
    brewer.pal(6, "Paired")[1]
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
      text = element_text(size=font_size, family = "Linux Biolinum"),
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
    Max = max(Value),
    Q1 = quantile(Value, probs=0.25),
    Q3 = quantile(Value, probs=0.75))
  
  d = d %>% group_by(Bench, VM) %>% mutate(
    Ideal = Threads * stat(subset(Value, Threads=="1")))

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

render_plot <- function(full, output_pdf, scalability_lines=FALSE,
                     legend.pos=c(0,1), ops=1, multiplier=1,
                     error_bars=FALSE, error_barsQ1Q3=FALSE,legend.size = 8, sequential.line = FALSE, legend.nrow = FALSE,
                     ylabel = "Throughput", y_breaks = waiver(), font_size=9,
                     facets=FALSE, facets_scales="fixed", manual_points=FALSE,
                     legend.order = waiver()) {
  plot = ggplot(data = full, aes(x=Threads, y=Median, group=VM, color=VM))
  
  if (sequential.line) {
    seq <- subset(full, VM=="Local")$Median
    plot = plot + geom_hline(yintercept = seq, linetype = "dashed", colour = get_color("Local"))
  }
  
  xbreaks = unique(full$Threads)
  xbreaks = subset(xbreaks, xbreaks!=22)
  
  plot = plot +
    geom_line(size=0.5) +
    my_theme(legend.pos=legend.pos, font_size=font_size, legend.size = legend.size) +
    xlab("Threads") + ylab(ylabel) +
    scale_x_continuous(breaks = xbreaks, minor_breaks = NULL) +
    scale_y_continuous(breaks=y_breaks, labels=function(x)x*ops/multiplier)

  if (error_bars) {
    plot = plot + geom_errorbar(aes(ymin=Min, ymax=Max))
  } else if (error_barsQ1Q3) {
    plot = plot + geom_errorbar(aes(ymin=Q1, ymax=Q3))
  }

  if (facets) {
    # plot = plot + coord_cartesian(xlim = c(2, max(full$Threads) - 1))
  } else {
    plot = plot + coord_cartesian(xlim = c(0, max(full$Threads) + 2), ylim = c(0, max(full$Value)*1.05), expand=FALSE)
  }

  if (!manual_points) {
    plot = plot + geom_point(size=1.5, aes(shape=VM)) +
      scale_shape_manual(values=c(16,17,15,4,1), breaks=legend.order)
  }

  # plot = plot + geom_line(data = subset(full, VM == "Local"), size=0.4, color="black", linetype=3)
  # plot = plot + scale_colour_brewer(palette = "Paired")
  plot = plot + scale_colour_manual(values = get_colors(levels(droplevels(full)$VM)), breaks=legend.order)

  if (!identical(legend.nrow, FALSE)) {
    plot = plot + guides(shape = guide_legend(nrow = legend.nrow,
                                              keyheight = 0.5))
    plot = plot + guides(colour = guide_legend(nrow = legend.nrow,
                                              keyheight = 0.5))
  }

  if (!identical(scalability_lines, FALSE)) {
    subdata = subset(full, VM==scalability_lines)
    plot = plot + geom_line(data = subdata, aes(x=Threads, y=Ideal), color="black", size=0.2)
  }
  
  if (facets) {
    plot = plot + facet_wrap(~ Bench, scales = facets_scales, ncol = 4)
  }
  plot + theme(strip.background = element_blank(), panel.border = element_blank())
}

half_width = 2.68428
half_height = 1.6

# r = load_data("x62_conc_reads_volatile1.csv", stat=median)
# rw10 = load_data("x62_conc_rw10_90_volatile1.csv", stat=max)
# rw50 = load_data("x62_conc_rw50_50_volatile1.csv", stat=max)
# w = load_data("x62_conc_writes_volatile1.csv", stat=max)

# LayoutLock results ar ebad, so use the old ones
r = rbind(subset(load_data("conc_reads_29mar.csv", stat=median), VM!="LayoutLock"),
             subset(load_data("x62_conc_reads_volatile2.csv", stat=median), VM=="LayoutLock"))
rw10 = rbind(subset(load_data("conc_reads_writes1090_29mar.csv", stat=max), VM!="LayoutLock"),
             subset(load_data("x62_conc_rw10_90_volatile2.csv", stat=max), VM=="LayoutLock"))
rw50 = rbind(subset(load_data("conc_reads_writes5050_29mar.csv", stat=max), VM!="LayoutLock"),
             subset(load_data("x62_conc_rw50_50_volatile2.csv", stat=max), VM=="LayoutLock"))
# w = load_data("x62_conc_writes_volatile2.csv", stat=max)

array_rw = rbind(r, rw10, rw50)
array_rw = rbind(subset(array_rw, VM=="Local"), subset(array_rw, VM!="Local"))
array_rw$Bench = recode(array_rw$Bench,
                    bench_array_conc_reads_ops.rb="100% reads",
                    bench_array_conc_write_reads_ops_10_90.rb="90% reads, 10% writes",
                    bench_array_conc_write_reads_ops_50_50.rb="50% reads, 50% writes",
                    bench_array_conc_write_ops.rb="100% writes")

render_plot(array_rw, ops=100*1000, multiplier=1e9, facets=TRUE, facets_scales="free_y", legend.pos="bottom", legend.nrow=2, font_size=8, legend.size=7,
            ylabel="Billion array accesses per sec.", manual_points=TRUE, scalability_lines = "Local") +
  small_facet_label +
  geom_point(aes(shape=VM, size=VM)) +
  scale_shape_manual(values=c(16,17,15,3,1,4,2)) +
  scale_size_manual(values=c(1.5,1.5,1.5,1.5,2.0,1.3,1.5)) +
  theme(legend.margin = margin(t=-0.65, b=0, l=0, r=0, unit="cm")) +
  theme(axis.title.x = element_text(size=7, hjust=0))
ggsave("conc_reads_writes.pdf", width = 5.48, height = 1.8, device = cairo_pdf)


gen_plot("x62_conc_appends3.csv", scalability_lines=FALSE, legend.pos=c(1, 0.1), legend.nrow=4, ops=1000, multiplier=1e6,
         ylabel="Million appends per second") +
  theme(axis.title.y = element_text(size=9, hjust=1))
ggsave("conc_appends.pdf", width = half_width, height = half_height, device = cairo_pdf)

safepoints = load_data("x62_safepoints2.csv")
safepoints$VM = recode(safepoints$VM, LightweightLayoutLock="SharedDynamicStorage")
render_plot(safepoints, scalability_lines=FALSE, ops=1000, multiplier=1e6, error_bars=TRUE, legend.pos=c(1,0),
         ylabel="Million reads per second") + theme(legend.key.size = unit(0.75, 'lines'))
ggsave("safepoints.pdf", width = half_width, height = half_height, device = cairo_pdf)

hash_ops = load_data("x62_hash_ops_1.csv", invert=TRUE)
render_plot(hash_ops, scalability_lines=FALSE, sequential.line = TRUE, ylabel="Scalability of Hash ops.", legend.pos=c(0,1.0), manual_points=TRUE) +
  geom_point(aes(shape=VM, size=VM)) + scale_shape_manual(values=c(16,17,15,4,5)) + scale_size_manual(values=c(1.5,1.5,1.5,1.5,2.5)) +
  theme(legend.key.size = unit(0.75, 'lines'))
ggsave("hash_ops_80_10_10.pdf", width = half_width, height = half_height, device = cairo_pdf)

group_by = load_data("group_by1.csv", invert=TRUE)
render_plot(group_by, scalability_lines=FALSE,
         sequential.line = TRUE, ylabel = "Scalability relative to Local", y_breaks = seq(0, 44, 1),
         legend.pos=c(0,1.0), manual_points=TRUE) +
  geom_point(aes(shape=VM, size=VM)) + scale_shape_manual(values=c(16,17,15,4,5)) + scale_size_manual(values=c(1.5,1.5,1.5,1.5,2.0)) +
  theme(axis.title.y = element_text(size=9, hjust=1),
        legend.key.size = unit(0.75, 'lines'))
ggsave("histogram_fixed_workload_40M_100K.pdf", width = half_width, height = half_height, device = cairo_pdf)


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

truffleruby = load_data("threaded_reverse_pool_fixed_truffleruby_both_jan4.csv", norm=TRUE)
truffleruby$VM = recode(truffleruby$VM, SharedFixedStorage="Concurrent Strategies", Local="TruffleRuby")
jruby = load_data("x62_threaded_reverse_jruby_10k.csv", norm=TRUE)
# Concurrent Strategies must be plot above TruffleRuby
threaded_reverse = rbind(subset(truffleruby, VM=="TruffleRuby"), subset(truffleruby, VM=="Concurrent Strategies"), jruby)
threaded_reverse$VM = fct_relevel(threaded_reverse$VM, "TruffleRuby", "Concurrent Strategies", "JRuby")
render_plot(threaded_reverse, error_bars=TRUE, ylabel = "Scalability", scalability_lines = FALSE, legend.pos=c(0,legendy),
            legend.order=c("Concurrent Strategies", "TruffleRuby", "JRuby")) + no_y_label +
  theme(legend.key.size = unit(0.75, 'lines'))
ggsave("threaded_reverse.pdf", width = 2.24, height = 1.5, device = cairo_pdf)

# NPB

java_raw = load_raw_data("npb_java_all3.csv")
fortran_raw = load_raw_data("npb_fortran3.csv")
ruby_raw = load_raw_data("npb_base_full.csv")
ruby_raw$VM = recode(ruby_raw$VM, Ruby="Concurrent Strategies", Local="TruffleRuby")
npb_full = rbind(subset(ruby_raw, VM=="TruffleRuby"), fortran_raw, java_raw, subset(ruby_raw, VM=="Concurrent Strategies"))

java = load_data("npb_java_all3.csv", warmup=TRUE, npb=TRUE)
fortran = load_data("npb_fortran3.csv", warmup=FALSE, npb=TRUE)
ruby = load_data("npb_base_full.csv", warmup=TRUE, npb=TRUE)
ruby$VM = recode(ruby$VM, Ruby="Concurrent Strategies", Local="TruffleRuby")

# Concurrent Strategies must be plot above TruffleRuby
npb = rbind(subset(ruby, VM=="TruffleRuby"), fortran, java, subset(ruby, VM=="Concurrent Strategies"))
npb$VM = fct_relevel(npb$VM, "TruffleRuby", "Concurrent Strategies", "Java", "Fortran")

render_plot(npb, "npb_scalability.pdf", facets=TRUE, facets_scales="fixed", error_bars=TRUE, ylabel="Scalability relative to 1 thread performance",
            legend.pos="bottom", legend.order=c("Concurrent Strategies", "TruffleRuby", "Java", "Fortran"), scalability_lines = "TruffleRuby") + small_facet_label +
  expand_limits(y=0) +
  theme(legend.key.size = unit(0.75, 'lines')) +
  theme(legend.margin = margin(t=-0.7, b=0, l=0, r=0, unit="cm")) +
  theme(axis.title.x = element_text(size=8, hjust=0))
ggsave("npb_scalability.pdf", width = 5.48, height = 2.7, device = cairo_pdf)

# Realistic Ruby benchmarks

sidekiq_sum = load_data("sidekiq_sum_23mar.csv", warmup=5, norm=TRUE)
sidekiq_sum$VM = recode(sidekiq_sum$VM, LightweightLayoutLock="Concurrent Strategies", Local="TruffleRuby")
render_plot(sidekiq_sum, error_bars=TRUE, scalability_lines = "TruffleRuby", ylabel = "Scalability", legend.pos=c(0,legendy)) + no_y_label +
  # theme(plot.margin = unit(c(0.1,0,0,0), "cm")) + # Some margin for the top y label
  theme(legend.key.size = unit(0.75, 'lines'))
ggsave("sidekiq_sum.pdf", width = 2.24, height = 1.5, device = cairo_pdf)

sidekiq_prawn = load_data("sidekiq_prawn_28mar.csv", warmup=30, norm=TRUE)
sidekiq_prawn_count = 10000
sidekiq_prawn$VM = recode(sidekiq_prawn$VM, SharedFixedStorage="Concurrent Strategies", Local="TruffleRuby")
render_plot(sidekiq_prawn, error_bars=TRUE, scalability_lines = "TruffleRuby", ylabel = "Scalability", legend.pos=c(0,legendy)) + # no_y_label +
  theme(legend.key.size = unit(0.75, 'lines'))
ggsave("sidekiq_prawn.pdf", width = 2.24, height = 1.6, device = cairo_pdf)

# full = rbind(sidekiq_sum, sidekiq_prawn)
# render_plot(full, error_bars=FALSE, scalability_lines = TRUE, ylabel = "Scalability", legend.pos=c(0,legendy)) + no_y_label +
#   theme(legend.key.size = unit(0.75, 'lines'))
# ggsave("sidekiq.pdf", width = 2.24, height = 1.5, device = cairo_pdf)


mri = load_data("webrick_mri7_nopool.csv", warmup=10)
jruby = load_data("webrick_jruby7_nopool.csv", warmup=10)
tr = load_data("webrick_truffleruby_both_jan3.csv", warmup=40)
tr$VM = recode(tr$VM, SharedFixedStorage="Concurrent Strategies", Local="TruffleRuby")
webrick = rbind(subset(tr, VM=="TruffleRuby"), subset(tr, VM=="Concurrent Strategies"), mri, jruby)
webrick$VM = fct_relevel(webrick$VM, "TruffleRuby", "Concurrent Strategies", "JRuby", "MRI")
webrick_mri1 = round(unique(subset(webrick, VM=="MRI" & Threads==1)$Median), -1)

render_plot(webrick, error_barsQ1Q3=TRUE, scalability_lines = FALSE, ylabel = "Requests per second", #legend.pos=c(-0.02,1.08),
            legend.order=c("Concurrent Strategies", "TruffleRuby", "JRuby", "MRI")) + #, y_breaks = c(webrick_mri1, seq(2000, 12000, 2000))) +
  scale_y_continuous(breaks = c(webrick_mri1, seq(2000, 12000, 2000)),  minor_breaks = NULL) +
  coord_cartesian(xlim = c(0, max(webrick$Threads) + 2), ylim = c(0, 15000), expand=FALSE) +
  theme(legend.key.size = unit(0.75, 'lines'))
ggsave("webrick.pdf", width = 4, height = 1.68, device = cairo_pdf)

### Are-We-Fast-Yet ###

load_awfy_file <- function(file, row_names) {
  row_names = c("TimeStamp", "Value", "Unit", "Criterion", "Benchmark",
                "VM", "Suite", "Extra", "Warmup", "Cores", "InputSize", "Var")
  d = read.table(file, sep="\t", header=FALSE, col.names=row_names, fill=TRUE)
  d$Benchmark = factor(d$Benchmark)
  # Add Iteration
  d = d %>% group_by(Benchmark, VM, Var, Extra, Cores, Suite) %>% mutate(
    Iteration = seq_len(length(Value)))

  # Rename
  d$VM = recode(d$VM, NoSharing="TruffleRuby", Sharing="Concurrent Strategies")

  data.frame(d)
}

transform_awfy <- function(d) {
  # Remove warmup
  #d = subset(d, d$Iteration>=200)

  # Stats
  d = d %>% group_by(Benchmark, VM) %>% mutate(
    Median = median(Value),
    Min = min(Value),
    Max = max(Value))

  # Normalize results with the baseline configuration
  d = d %>% group_by(Benchmark) %>% mutate(
    Norm = Value / median(subset(Value, VM=="TruffleRuby")))

  data.frame(d)
}

small = load_awfy_file("awfy_small1.csv")
big = load_awfy_file("awfy_big1.csv")
awfy_full = rbind(small, big)

awfy = transform_awfy(awfy_full)

awfy_breaks <- levels(droplevels(awfy)$VM)
awfy_colors <- get_colors(awfy_breaks)

ggplot(data = awfy, aes(x=Benchmark, y=Norm, color=VM)) +
  geom_boxplot(outlier.size=0.3,lwd=0.5) +
  my_theme(legend.pos="right") +
  ylab("Runtime factor") +
  xlab("") +
  scale_colour_manual(values = awfy_colors) +
  coord_cartesian(ylim = c(0.9, 1.4), expand=FALSE, xlim = c(0.5,length(levels(factor(awfy$Benchmark)))+0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        plot.margin = unit(c(0.15,0,0,0.1), "cm"),
        strip.background = element_blank(), panel.border = element_blank(),
        legend.margin=margin(t = -0.5, unit='cm')) # Some margin for the top y label

ggsave("awfy.pdf", width = 5.48, height = 1.8, device = cairo_pdf)

# WARMUP

# AWFY warmup
data <- awfy_full %>% group_by(Benchmark) %>% mutate(
  Ratio = Value / median(subset(Value, VM=="TruffleRuby")))
max_iter = 300
data <- subset(data, Iteration <= max_iter)

ggplot(data, aes(x=Iteration, y=Ratio)) +
  geom_line(aes(colour = VM), size=0.2) +
  my_theme(legend.pos="none") +
  theme(strip.background = element_blank(), panel.border = element_blank()) +
  facet_wrap(~ Benchmark, nrow = 2) +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 400, 600, 800, 1000)) +
  scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2)) +
  coord_cartesian(ylim = c(0.8, 2), xlim = c(-10,max_iter-1), expand=FALSE) +
  scale_colour_manual(values = awfy_colors) +
  theme(strip.text = element_text(size = 8)) +
  theme(panel.grid.minor = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size=1))) +
  ylab("Runtime per iteration")

ggsave("awfy_warmup.pdf", width = 5.48, height = 2, device = cairo_pdf)


# NPB warmup
npb_warmup = subset(npb_full, Threads==8)
npb_warmup$Iteration = npb_warmup$Iteration+1
npb_warmup = subset(npb_warmup, VM=="TruffleRuby" | VM=="Concurrent Strategies")
data <- npb_warmup %>% group_by(Bench) %>% mutate(
  Ratio = Value / median(subset(Value, VM=="TruffleRuby")))
max_iter = 10
data <- subset(data, Iteration <= max_iter)

ggplot(data, aes(x=Iteration, y=Ratio)) +
  geom_line(aes(colour = VM), size=0.2) +
  my_theme(legend.pos="bottom") +
  theme(strip.background = element_blank(), panel.border = element_blank()) +
  facet_wrap(~ Bench, nrow = 2, scales = "free_y") +
  scale_x_continuous(breaks = c(0, 1, 2, 4, 6, 8, 10)) +
  #scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2)) +
  coord_cartesian(xlim=c(1, 10), expand=FALSE) +
  scale_colour_manual(values = awfy_colors) +
  theme(strip.text = element_text(size = 8)) +
  theme(panel.grid.minor = element_blank()) +
  #theme(legend.position = c(0.9,0)) +
  theme(legend.margin = margin(t=-0.8, b=0, l=0, r=0, unit="cm")) +
  theme(plot.margin = margin(l=0.1, r=0.2, unit="cm")) +
  #theme(legend.justification = "right") +
  theme(axis.title.x = element_text(size=8, hjust=0)) +
  guides(colour = guide_legend(override.aes = list(size=.7))) +
  ylab("Runtime per iteration")

ggsave("npb_warmup.pdf", width = 5.48, height = 1.8, device = cairo_pdf)

# STATS

unlink("data.tex")

write_stat = function(name, value) {
  write(paste0("\\def\\stats", name, "{", value, "}"), "data.tex", append = TRUE)
}

awfySafeOverUnsafe = awfy %>% group_by(Benchmark) %>% summarize(
  SafeOverUnsafe = unique(subset(Median, VM=="Concurrent Strategies")) / unique(subset(Median, VM=="TruffleRuby"))
)

write_stat("AWFYLargestSpeedupPercents", round((1-min(awfySafeOverUnsafe$SafeOverUnsafe))*100))
write_stat("AWFYLargestSpeedupBenchmark", subset(awfySafeOverUnsafe, SafeOverUnsafe == min(SafeOverUnsafe))$Benchmark)

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

arraySafeOverUnsafe = subset(array_rw, Threads==1) %>% group_by(Bench) %>% summarize(
  SafeOverUnsafe = unique(subset(Median, VM=="SharedFixedStorage")) / unique(subset(Median, VM=="Local"))
)

table_data = subset(array_rw, Threads==1 | Threads==44) %>%
  group_by(Bench, VM, Threads) %>% summarise(
    Value = unique(Median)
  ) %>% group_by(Bench, Threads) %>% mutate(
    Ratio = Value / subset(Value, VM=="Local")
  ) %>% group_by(Bench, VM) %>% mutate(
    Scalability = Value / subset(Value, Threads==1)
  )
write.csv(table_data, file="array_rw_data.csv")

npb_speedups = subset(npb, Threads==1) %>% group_by(Bench, VM, Threads) %>% summarise(
  Value = median(Orig)
) %>% group_by(Bench, Threads) %>% mutate(
  RubyOverJava = subset(Value, VM=="Concurrent Strategies") / max(subset(Value, VM=="Java")),
  JavaOverFortran = subset(Value, VM=="Java") / max(subset(Value, VM=="Fortran")),
  RubyOverFortran = subset(Value, VM=="Concurrent Strategies") / max(subset(Value, VM=="Fortran")),
  SafeOverUnsafe = subset(Value, VM=="Concurrent Strategies") / max(subset(Value, VM=="TruffleRuby"))
) %>% group_by(Bench) %>% summarize(
  MinRubyOverJava = min(RubyOverJava),
  MaxRubyOverJava = max(RubyOverJava),
  MedianRubyOverJava = median(RubyOverJava),

  MinJavaOverFortran = min(JavaOverFortran),
  MaxJavaOverFortran = max(JavaOverFortran),
  MedianJavaOverFortran = median(JavaOverFortran),

  MinRubyOverFortran = min(RubyOverFortran),
  MaxRubyOverFortran = max(RubyOverFortran),
  MedianRubyOverFortran = median(RubyOverFortran),
  
  MedianSafeOverUnsafe = median(SafeOverUnsafe)
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

npb_warmup_iter1 = subset(npb_warmup, Iteration==1)
npb_warmup_iter1 <- npb_warmup_iter1 %>% group_by(Bench) %>% mutate(Ratio = Value / subset(Value, VM=="TruffleRuby"))
npb_warmup_iter1 = subset(npb_warmup_iter1, VM=="Concurrent Strategies")$Ratio
write_stat("NPBWarmupIterOneOverhead", round((geomean(npb_warmup_iter1)-1) * 100))
write_stat("NPBWarmupIterOneMin", round((min(npb_warmup_iter1)-1) * 100))
write_stat("NPBWarmupIterOneMax", round((max(npb_warmup_iter1)-1) * 100))

npb_warmup_iter4 = subset(npb_warmup, Iteration==4)
npb_warmup_iter4 <- npb_warmup_iter4 %>% group_by(Bench) %>% mutate(Ratio = Value / subset(Value, VM=="TruffleRuby"))
npb_warmup_iter4 = subset(npb_warmup_iter4, VM=="Concurrent Strategies")$Ratio
geomean4 = geomean(npb_warmup_iter4)

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
mandelSafeOverUnsafe = unique(subset(mandel, VM=="Concurrent Strategies" & Threads==1)$Median/local1)

local1 = subset(hash_ops, VM=="Local" & Threads==1)$Median
ll = subset(hash_ops, VM=="LayoutLock" & Threads==1)$Median
lll = subset(hash_ops, VM=="LightweightLayoutLock" & Threads==1)$Median
lll44 = subset(hash_ops, VM=="LightweightLayoutLock" & Threads==44)$Median
write_stat("HashOpsOverheadLL", round((unique(local1/ll)-1)*100))
write_stat("HashOpsOverheadLLL", round((unique(local1/lll)-1)*100))
write_stat("HashOpsMaxSpeedup", round(unique(lll44/local1)))
lll44ops = median(subset(hash_ops, VM=="LightweightLayoutLock" & Threads==44)$Orig)
write_stat("HashOpsMaxOpsLLL", round(lll44ops))
hashOpsSafeOverUnsafe = unique(subset(hash_ops, VM=="LightweightLayoutLock" & Threads==1)$Median) / unique(subset(hash_ops, VM=="Local" & Threads==1)$Median)

groupBySafeOverUnsafe = unique(subset(group_by, VM=="LightweightLayoutLock" & Threads==1)$Median) / unique(subset(group_by, VM=="Local" & Threads==1)$Median)

# d = subset(npb, Threads==1) %>% group_by(Bench, VM) %>% summarize(MinTime=min(Orig), MaxTime=max(Orig))
d = subset(npb, Threads==1) %>% summarize(MinTime=min(Orig), MaxTime=max(Orig))
write_stat("NPBMinSingleThreadTime", round(d$MinTime))
write_stat("NPBMaxSingleThreadTime", round(d$MaxTime/60))

sidekiq_sum_unsafe1 = median(subset(sidekiq_sum, VM=="TruffleRuby" & Threads==1)$Orig)
sidekiq_sum_cs1 = median(subset(sidekiq_sum, VM=="Concurrent Strategies" & Threads==1)$Orig)
sidekiq_sumSafeOverUnsafe = sidekiq_sum_cs1/sidekiq_sum_unsafe1

sidekiq_prawn_unsafe1 = median(subset(sidekiq_prawn, VM=="TruffleRuby" & Threads==1)$Orig)
sidekiq_prawn_cs1 = median(subset(sidekiq_prawn, VM=="Concurrent Strategies" & Threads==1)$Orig)
sidekiq_prawnSafeOverUnsafe = sidekiq_prawn_cs1/sidekiq_prawn_unsafe1

sidekiq_prawn_unsafe = subset(sidekiq_prawn, VM=="TruffleRuby")
sidekiq_prawn_cs = subset(sidekiq_prawn, VM=="Concurrent Strategies")
speedup = median(subset(sidekiq_prawn_cs, Threads==16)$Value)
write_stat("PrawnSpeedupSixteen", round(speedup))
pdfs_per_sec1 = sidekiq_prawn_count*median(subset(sidekiq_prawn_cs, Threads==1)$Orig)
pdfs_per_sec16 = sidekiq_prawn_count*median(subset(sidekiq_prawn_cs, Threads==16)$Orig)
write_stat("PrawnPDFsPerSec", round(pdfs_per_sec16))
prawn_overhead = sidekiq_prawn %>% group_by(Threads) %>% summarize(
  SafeOverUnsafe = median(subset(Value, VM=="Concurrent Strategies")) / median(subset(Value, VM=="TruffleRuby"))
)
write_stat("PrawnMaxOverhead", round((1-min(prawn_overhead$SafeOverUnsafe))*100))
write_stat("PrawnMinOverhead", round((1-max(prawn_overhead$SafeOverUnsafe))*100))
rd=84913340
wr=51955560
write_stat("PrawnRdPerSec", round(rd/sidekiq_prawn_count*pdfs_per_sec1/1e6, 1))
write_stat("PrawnWrPerSec", round(wr/sidekiq_prawn_count*pdfs_per_sec1/1e6, 1))

jruby1 = median(subset(threaded_reverse, Threads==1 & VM=="JRuby")$Orig)
truby1 = median(subset(threaded_reverse, Threads==1 & VM=="Concurrent Strategies")$Orig)
unsafe1 = median(subset(threaded_reverse, Threads==1 & VM=="TruffleRuby")$Orig)
speedup1 = truby1/jruby1
write_stat("ThreadedReverseSpeedupOne", round(speedup1))
reverseThreadedSafeOverUnsafe = truby1/unsafe1

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
unsafe1 = median(subset(webrick, Threads==1 & VM=="TruffleRuby")$Orig)
truby1 = median(subset(webrick, Threads==1 & VM=="Concurrent Strategies")$Orig)
webrickSafeOverUnsafe = truby1/unsafe1

allSafeOverUnsafe = c(
  awfySafeOverUnsafe$SafeOverUnsafe,
  # not micros
  #arraySafeOverUnsafe$SafeOverUnsafe,
  #hashOpsSafeOverUnsafe,
  #groupBySafeOverUnsafe,
  1/npb_speedups$MedianSafeOverUnsafe,
  mandelSafeOverUnsafe, reverseThreadedSafeOverUnsafe, sidekiq_sumSafeOverUnsafe, sidekiq_prawnSafeOverUnsafe, webrickSafeOverUnsafe)

#length(allSafeOverUnsafe)
#allSafeOverUnsafe

write_stat("GeoMeanAll", round((1-geomean(allSafeOverUnsafe))*100))
write_stat("MinAll", round((1-min(allSafeOverUnsafe))*100))
write_stat("MaxAll", round((max(allSafeOverUnsafe)-1)*100))

