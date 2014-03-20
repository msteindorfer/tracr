#!/usr/bin/env Rscript

require(sitools)

# http://stackoverflow.com/questions/11340444/is-there-an-r-function-to-format-number-using-unit-prefix
csUnits <- function (number,rounding=T) 
{
  lut <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12, 1e-09, 1e-06, 
           0.001, 1, 1000, 1e+06, 1e+09, 1e+12, 1e+15, 1e+18, 1e+21, 
           1e+24)
  pre <- c("y", "z", "a", "f", "p", "n", "u", "m", "", "K", 
           "M", "G", "T", "P", "E", "Z", "Y")
  ix <- findInterval(number, lut)
  if (lut[ix]!=1) {
    if (rounding==T) {
      sistring <- paste(formatC(number/lut[ix], digits=0, format="f"),pre[ix], sep="")
    }
    else {
      sistring <- paste(formatC(number/lut[ix], digits=0, format="f"), pre[ix], sep="")
    } 
  }
  else {
    sistring <- paste(round(number, digits=0))
  }
  return(sistring)
}

tableColumnNames <- c(
    "BenchmarkShortName"
  , "ObjAllocations"
  , "MemOriginal"
  , "MemEstimated"
  , "MemMeasurementA"                 
  , "MemMeasurementB"                 
)

memoryProfilesS <- read.csv("calibration-redundant-data.csv", header=FALSE, col.names = tableColumnNames)
memoryProfilesSStressed <- read.csv("calibration-redundant-data-stressed.csv", header=FALSE, col.names = tableColumnNames)

common_xlab = 'object count (log) [by tree depth]'
common_ylab = 'memory usage (log)'

xAt = c(0, 1, 2, 5, 10, 15, 20)
xTicksLog2 = log(2^(xAt+1)-1+3, base = 2)

xAxis <- function() {
  axis(1, at = xTicksLog2, labels = paste(lapply(memoryProfilesS$ObjAllocations, csUnits), " [d=", xAt , "]", sep = ""), cex.axis=0.85)
}  

yAxis <- function() {
  yAt = c(100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)
  axis(2, at = log10(yAt), labels = lapply(yAt, csUnits), cex.axis=0.85)
}

pdf("calibrationRedundant.pdf", width=7, height=5)  
  xRange <- range(log(memoryProfilesS$ObjAllocations, base = 2))
  yRange <- range(log10(memoryProfilesS$MemOriginal), log10(memoryProfilesS$MemEstimated))
  
  plot(memoryProfilesS$ObjAllocations, type = "n", xlab=common_xlab, xaxt = "n", xlim = xRange, ylab=common_ylab, ylim = yRange, yaxt = "n")
  
  legend('topleft', c('original', 'estimate', 'measurement (with default heap size)', 'measurement (with minimal heap size)'), 
       lty = c(1, 3, 1, 1), bty='n', cex=.75, pch = c(19, 17, 1, 13))

  #xLabels = parse(text=paste("2^(", xAt, ")-1", sep=""))
  xAxis()
  yAxis()  

  points(x = xTicksLog2, y = log10(memoryProfilesS$MemOriginal), type="b", pch=19)
  points(x = xTicksLog2, y = log10(memoryProfilesS$MemEstimated), type="b", pch=17, lty=3)
  points(x = xTicksLog2, y = log10(memoryProfilesS$MemMeasurementA), type="b", pch=1)
  #points(log10(memoryProfilesS$MemMeasurementB), type="b", pch=2)  
  points(x = xTicksLog2, y = log10(memoryProfilesSStressed$MemMeasurementA), type="b", pch=13)
dev.off()


memoryProfilesU <- read.csv("calibration-redundancy-free-data.csv", header=FALSE, col.names = tableColumnNames)

pdf("calibrationRedundancyFree.pdf", width=7, height=5)
  yRange <- range(log10(memoryProfilesU$MemOriginal), log10(memoryProfilesU$MemEstimated))

  plot(memoryProfilesS$ObjAllocations, type = "n", xlab=common_xlab, xaxt = "n", xlim = xRange, ylab=common_ylab, ylim = yRange, yaxt = "n")

  legend('topleft', c('original', 'estimate', 'measurement (with default heap size)'), 
       lty = c(1, 3, 1), bty='n', cex=.75, pch = c(19, 17, 1))

  #axis(1, x <- seq(from=1, to=length(memoryProfilesU$ObjAllocations)), labels = lapply(memoryProfilesU$ObjAllocations, csUnits))
  #axis(2, y <- seq(from=0, to=max(yRange), by=(max(yRange) - min(yRange)) / 4), labels = lapply(exp(y), csUnits))
  
  xAxis()
  yAxis()

  points(x = xTicksLog2, y = log10(memoryProfilesU$MemOriginal), type="b", pch=19)
  points(x = xTicksLog2, y = log10(memoryProfilesU$MemEstimated), type="b", pch=17, lty=3)
  points(x = xTicksLog2, y = log10(memoryProfilesU$MemMeasurementA), type="b", pch=1)
  #points(log10(memoryProfilesU$MemMeasurementB), type="b", pch=2)  
dev.off()