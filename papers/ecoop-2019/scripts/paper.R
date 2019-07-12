if (Sys.getenv("RSTUDIO") != "1") { 
  opts_chunk$set(
      fig.path="../generated/",
      fig.keep='all',
      dev=c('tikz'), # , 'svg'
      dev.args=list(pointsize=8, timestamp = FALSE),
      #dev='pdf',c('tikz', 'svg'),
      echo=FALSE,
      external=FALSE,
      tidy=FALSE)

  ## Make sure that TikZDevice is used for measuring size of latex labels
  options(device = function(...) tikzDevice::tikz(tempfile(), ...))
}

## Determine by inspection of warmup-plot.Rmd
warmup_low <- 25
warmup_low_fast <- 350
warmup_high <- 990

vm_names <- c(
  "GraalBasic"            = "Graal",
  "GraalEnterprise"       = "Graal VM",
  
  "Java"              = "Java",
  
  "Node"                  = "Node.js (V8)",
  "SOMns-no-tracing"      = "SOMns",
  "SOMns"                 = "SOMns",
  "SOMns-Enterprise"      = "SOMns GraalVM",
  
  "Higgs"      = "Higgs",
  "Moth (untyped)"  = "Moth",
  "MothTyped" = "Fully Typed")

vms_all <- names(vm_names)

# vm_colors <- brewer.pal(length(vms_all), "Paired")  # to replace scale_fill_brewer(type = "qual", palette = "Paired")
vm_colors <- rainbow(length(vms_all))

names(vm_colors) <- vm_names

per <- function (x) {
  round((x - 1) * 100, 0)
}

X0 <- function(x) {
  round(x, 0)
}

X1 <- function(x) {
  round(x, 1)
}

X2 <- function(x) {
  round(x, 2)
}
