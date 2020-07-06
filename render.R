#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
input_file  <- args[1]
output_file <- args[2]
output_dir  <- args[3]

if (endsWith(output_file, ".md")) {
  library(knitr)
  knit(input_file, paste0(output_dir, '/', output_file))
} else {
  library(rmarkdown)
  render(input_file,
         "all",
         output_file,
         output_dir = output_dir,
         runtime = "static", clean = TRUE)
}
