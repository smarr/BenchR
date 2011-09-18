#!/usr/bin/env Rscript

### Using R to Understand Benchmarking Results
# This script is part of the following blog post:
# http://www.stefan-marr.de/2011/09/using-r-to-understand-benchmarking-results
# License: http://www.opensource.org/licenses/mit-license.php

### Load and install libraries if necessary
if (!library(plyr, logical.return=TRUE)) {
  install.packages("plyr")
  library(plyr)
}
if (!library(beanplot, logical.return=TRUE)) {
  install.packages("beanplot")
  library(beanplot)
}
if (!library(doBy, logical.return=TRUE)) {
  install.packages("doBy")
  library(doBy)
}

### Load a data file

# For this to work in RStudio, you have to set setwd("folder-with-data-file")
bench <- read.table("object-table-data.csv.bz2",
                    sep="\t",
                    header=FALSE,
                    col.names=c("Time", "Benchmark", "VirtualMachine",
                                "Platform", "ExtraArguments", "Cores",
                                "Iterations", "None", "Criterion",
                                "Criterion-total"),
                    fill=TRUE)

### Transforming input data

# Splitting information encoded in a string into separate columns
bench <- ddply(bench,
               ~ VirtualMachine, # this formula groups the data by the value in VirtualMachine
               transform,
               # the second part of the VM name indicates whether it uses the object table
               ObjectTable = strsplit(as.character(VirtualMachine), "-")[[1]][2] == "OT",
               # the third part indicates the format of the object header
               Header = factor(strsplit(as.character(VirtualMachine), "-")[[1]][3]))

# Subsetting the data to concentrate on the relevant data points
bench <- subset(bench,
                Header == "full"         # concentrate on the VM with full object headers
                 & Criterion == "total", # use only total values of a measurement
                select=c(Time, Platform, # use only a limited number of columns
                         ObjectTable, Benchmark, ExtraArguments, Cores))

### Normalizing the data

# Order the results for further processing
bench <- orderBy(~Time + Platform + ObjectTable + Benchmark + ExtraArguments + Cores, bench)

# Normalize the data based on the VM that is using an object table
norm_bench <- ddply(bench, ~ Platform + Benchmark + ExtraArguments + Cores,
                    transform,
                    SpeedRatio = Time / Time[ObjectTable == TRUE])

# Now just drop all the stuff we do not need
norm_bench <- subset(norm_bench, ObjectTable == FALSE, c(SpeedRatio, Platform, Benchmark, ExtraArguments, Cores))

### Analyzing the data

# Some basic queries on the data
summary(norm_bench$SpeedRatio)
sd(norm_bench$SpeedRatio)
mean(norm_bench$SpeedRatio)
median(norm_bench$SpeedRatio)

# Some slightly less basic queries
summary(norm_bench$SpeedRatio[norm_bench$Cores==1])
summary(norm_bench$SpeedRatio[norm_bench$Cores==16])

# Overview of overall SpeedRatios distribution
beanplot(SpeedRatio ~ Platform,
         data = norm_bench,
         what = c(1,1,1,0), log="",
         ylab="Runtime: noOT/OT")

# We might investigate further the impact of the number of cores
beanplot(SpeedRatio ~ Cores,
         data = norm_bench,
         what = c(1,1,1,0), log="",
         ylab="Runtime: noOT/OT")
         
# Do the irregular distributions come from benchmark specific results?
beanplot(SpeedRatio ~ Benchmark,
         data = subset(norm_bench, Cores==2),
         what = c(1,1,1,0), log="",
         ylab="Runtime: noOT/OT",
         las=2)

beanplot(SpeedRatio ~ Benchmark,
         data = subset(norm_bench, Cores==16),
         what = c(1,1,1,0), log="",
         ylab="Runtime: noOT/OT",
         las=2)

# Get names of benchmarks
levels(norm_bench$Benchmark)

# Filter for SMarkLoops.benchFloatLoop
beanplot(SpeedRatio ~ Cores,
         data = subset(norm_bench, Benchmark == "SMarkLoops.benchFloatLoop"),
         what = c(1,1,1,0), log="",
         ylab="Runtime: noOT/OT")

# Add splitting by ExtraArguments
beanplot(SpeedRatio ~ Cores + ExtraArguments,
         data = subset(norm_bench, Benchmark == "SMarkLoops.benchFloatLoop"),
         what = c(1,1,1,0), log="",
         ylab="Runtime: noOT/OT",
         las=2)

# Drop unused factor levels to have a concise plot
beanplot(SpeedRatio ~ Cores + ExtraArguments,
         data = droplevels(subset(norm_bench, Benchmark == "SMarkLoops.benchFloatLoop")),
         what = c(1,1,1,0), log="",
         ylab="Runtime: noOT/OT",
         las=2)

### Fixing up a mistake

# What exactly are those three ExtraArguments used here?
summary(droplevels(subset(norm_bench, Benchmark == "SMarkLoops.benchFloatLoop")))
levels(droplevels(subset(norm_bench, Benchmark == "SMarkLoops.benchFloatLoop"))$ExtraArguments)
levels(norm_bench$ExtraArguments)

# Filtering out uninterersting benchmarks
norm_bench <- subset(norm_bench,
                     !grepl("^1 ", ExtraArguments)    # those beginning with "1 " put load on a single core
                     & !grepl("s0 ", ExtraArguments)) # those having "s0 " in it put 10x load on each core
norm_bench <- droplevels(norm_bench)

### Overview Graph

par(mar=c(20, 4, 1, 1))
beanplot(SpeedRatio ~ Cores + Benchmark,
         data = norm_bench,
         what = c(1,1,1,0), log="",
         ylab="Runtime: noOT/OT", las=2)

