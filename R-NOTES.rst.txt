Notes on How To use R
---------------------

Load a Rebench CSV file:
  > read.table("/Users/smarr/Projects/PhD/IBM/notes/scheduling-issue/intel.data.csv", sep="\t", header=TRUE)

  - I added the header manually, see data.csv.header
    it used to be (tab-separated):
    Time	Benchmark	Virtual Machine	Platform	Extra Arguments	Cores	Iterations	None	Criterion
  - Tell R that cores etc. are factors (parameters that are varied)
    http://www.cyclismo.org/tutorial/R/types.html
    bench$Cores <- factor(bench$Cores)
	bench$Iterations <- factor(bench$Iterations)



Help
 help(functionname) or ?functionname

Install Beanplot Package:

 > install.packages("beanplot", repos = "http://cran.r-project.org/")

IDE: www.rstudio.org


More packages:

install.packages("doBy", repos = "http://cran.r-project.org/")
install.packages("ggplot2")
install.packages("R.oo")
install.packages("stringr")
install.packages("psych") # for geometric.mean
install.packages("vioplot")


library(R.oo)
library(vioplot)
library("stringr")

library(ggplot2)  # for line graphs with error bars

> attach(bench)
> library(doBy)
> summaryBy(Time ~ Benchmark + Extra.Arguments + Cores, bench)
> summaryBy(Time ~ Benchmark + Extra.Arguments + Cores, bench, FUN = function(x) { c(mean = mean(x), median = median(x), sd = sd(x)) })
> splittedBench <- splitBy(formula = Time ~ Benchmark + Extra.Arguments, data = bench)
> boxplot(Time ~ Benchmark + Extra.Arguments + Cores, bench, data = bench)
> xrange <- range(as.numeric(levels(bench$Cores)[bench$Cores]))
> yrange <- range(bench$Time)
> nbench <- length(splittedBench)
> plot(xrange, yrange, type="n", xlab="Cores", ylab="Time (ms)" )
> colors <- rainbow(nbench)
> linetype <- c(1:nbench) 
> plotchar <- seq(18, 18+nbench, 1)

for (i in 1:nbench) { 
  benchPart <- splittedBench[i]
  name = names(benchPart)
  bp = benchPart[[name]]
  lines(levels(bp$Cores), bp$Time, type="b", lwd=1.5, lty=linetype[i], col=colors[i], pch=plotchar[i]) 
}

See the .R scripts for more actual use