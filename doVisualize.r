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

logBase <- exp(1)
pdf("calibrationRedundant.pdf", width=7, height=5)
  yRange <- range(log(memoryProfilesS$MemOriginal, base=logBase), log(memoryProfilesS$MemEstimated, base=logBase))
  
  plot(memoryProfilesS$MemOriginal, type = "n", xlab='Allocation Count', xaxt = "n", ylab='Memory Usage', ylim = yRange, yaxt = "n")
  
  legend('topleft', c('Original', 'Estimate', 'Measurement (with default heap size)', 'Measurement (with minimal heap size)'), 
       lty = c(1, 3, 1, 1), bty='n', cex=.75, pch = c(19, 17, 1, 13))

  axis(1, x <- seq(from=1, to=length(memoryProfilesS$ObjAllocations)), labels = lapply(memoryProfilesS$ObjAllocations, csUnits))
  axis(2, y <- seq(from=0, to=max(yRange), by=(max(yRange) - min(yRange)) / 4), labels = lapply(exp(y), csUnits))

  points(log(memoryProfilesS$MemOriginal, base=logBase), type="b", pch=19)
  points(log(memoryProfilesS$MemEstimated, base=logBase), type="b", pch=17, lty=3)
  points(log(memoryProfilesS$MemMeasurementA, base=logBase), type="b", pch=1)
  #points(log(memoryProfilesS$MemMeasurementB, base=logBase), type="b", pch=2)  
  points(log(memoryProfilesSStressed$MemMeasurementA, base=logBase), type="b", pch=13)
dev.off()


memoryProfilesU <- read.csv("calibration-redundancy-free-data.csv", header=FALSE, col.names = tableColumnNames)

pdf("calibrationRedundancyFree.pdf", width=7, height=5)
  yRange <- range(log(memoryProfilesU$MemOriginal, base=logBase), log(memoryProfilesU$MemEstimated, base=logBase))

  plot(memoryProfilesU$MemOriginal, type = "n", xlab='Allocation Count', xaxt = "n", ylab='Memory Usage', ylim = yRange, yaxt = "n")

  legend('topleft', c('Original', 'Estimate', 'Measurement (with default heap size)'), 
       lty = c(1, 3, 1), bty='n', cex=.75, pch = c(19, 17, 1))

  axis(1, x <- seq(from=1, to=length(memoryProfilesU$ObjAllocations)), labels = lapply(memoryProfilesU$ObjAllocations, csUnits))
  axis(2, y <- seq(from=0, to=max(yRange), by=(max(yRange) - min(yRange)) / 4), labels = lapply(exp(y), csUnits))

  points(log(memoryProfilesU$MemOriginal, base=logBase), type="b", pch=19)
  points(log(memoryProfilesU$MemEstimated, base=logBase), type="b", pch=17, lty=3)
  points(log(memoryProfilesU$MemMeasurementA, base=logBase), type="b", pch=1)
  #points(log(memoryProfilesU$MemMeasurementB, base=logBase), type="b", pch=2)  
dev.off()