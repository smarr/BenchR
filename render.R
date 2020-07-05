#!/usr/bin/env Rscript
library(rmarkdown)
# library(knitr)
args <- commandArgs(trailingOnly = TRUE)
# This is what we used to use
# result_file <- knit2html(args[1], args[2])

# Now, they suggest this:
render(args[1],
       "all",
       args[2],
       output_dir = args[3],
       runtime = "static", clean = TRUE)
