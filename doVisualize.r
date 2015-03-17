#!/usr/bin/env Rscript
setwd("~/Development/rascal-devel/tracr")

require(sitools)
require(functional)

# http://stackoverflow.com/questions/11340444/is-there-an-r-function-to-format-number-using-unit-prefix
# csUnits <- function (number,rounding=T) 
# {
#   lut <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12, 1e-09, 1e-06, 
#            0.001, 1, 1000, 1e+06, 1e+09, 1e+12, 1e+15, 1e+18, 1e+21, 
#            1e+24)
#   pre <- c("y", "z", "a", "f", "p", "n", "u", "m", "", "K", 
#            "M", "G", "T", "P", "E", "Z", "Y")
#   ix <- findInterval(number, lut)
#   if (lut[ix]!=1) {
#     if (rounding==T) {
#       sistring <- paste(formatC(number/lut[ix], digits=0, format="f"),pre[ix], sep="")
#     }
#     else {
#       sistring <- paste(formatC(number/lut[ix], digits=0, format="f"), pre[ix], sep="")
#     } 
#   }
#   else {
#     sistring <- paste(round(number, digits=0))
#   }
#   return(sistring)
# }

csUnits <- function (number,rounding=T,postfix="") 
{
  lut <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12, 1e-09, 1e-06, 
           0.001, 1, 1000, 1e+06, 1e+09, 1e+12, 1e+15, 1e+18, 1e+21, 
           1e+24)
  pre <- c("y", "z", "a", "f", "p", "n", "u", "m", "", "K", 
           "M", "G", "T", "P", "E", "Z", "Y")
  ix <- findInterval(number, lut)
  if (!is.na(ix) && ix >= 1) {
    #     if (rounding==T) {
    #       sistring <- paste(formatC(number/lut[ix], digits=0, format="f"),pre[ix], sep="")
    #     }
    #     else {
    #       sistring <- paste(formatC(number/lut[ix], digits=0, format="f"), pre[ix], sep="")
    #     } 
    
    sistring <- paste(formatC(number/lut[ix], digits=0, format="f"), pre[ix], postfix, sep="")
  }
  else {
    sistring <- paste(round(number, digits=0), postfix, sep="")
  }
  
  return(sistring)
}

csUnitsNew <- Vectorize(csUnits)

csUnitsNew_Bytes <- Curry(csUnitsNew, postfix="B")


tableColumnNames <- c(
    "BenchmarkShortName"
  , "ObjAllocations"
  , "MeanMemOriginal"
  , "MeanMemEstimated"
  , "MeanMemMeasurementA"                 
  , "MeanMemMeasurementB"                 
)

memoryProfilesS <- read.csv("calibration-redundant-data.csv", header=FALSE, col.names = tableColumnNames)
memoryProfilesSStressed <- read.csv("calibration-redundant-data-stressed.csv", header=FALSE, col.names = tableColumnNames)

common_xlab = 'object count (log2) [d=tree depth]'
common_ylab = 'memory usage in bytes (log10)'

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
  yRange <- range(log10(memoryProfilesS$MeanMemOriginal), log10(memoryProfilesS$MeanMemEstimated))
  
  plot(memoryProfilesS$ObjAllocations, type = "n", xlab=common_xlab, xaxt = "n", xlim = xRange, ylab=common_ylab, ylim = yRange, yaxt = "n")
  
  legend('topleft', c('original', 'estimate', 'measurement (with default heap size)', 'measurement (with tight heap size)'), 
       lty = c(1, 3, 1, 1), bty='n', cex=.75, pch = c(19, 17, 1, 13))

  #xLabels = parse(text=paste("2^(", xAt, ")-1", sep=""))
  xAxis()
  yAxis()  

  points(x = xTicksLog2, y = log10(memoryProfilesS$MeanMemOriginal), type="b", pch=19)
  points(x = xTicksLog2, y = log10(memoryProfilesS$MeanMemEstimated), type="b", pch=17, lty=3)
  points(x = xTicksLog2, y = log10(memoryProfilesS$MeanMemMeasurementA), type="b", pch=1)
  #points(log10(memoryProfilesS$MeanMemMeasurementB), type="b", pch=2)  
  points(x = xTicksLog2, y = log10(memoryProfilesSStressed$MeanMemMeasurementA), type="b", pch=13)
dev.off()


memoryProfilesU <- read.csv("calibration-redundancy-free-data.csv", header=FALSE, col.names = tableColumnNames)

pdf("calibrationRedundancyFree.pdf", width=7, height=5)
  yRange <- range(log10(memoryProfilesU$MeanMemOriginal), log10(memoryProfilesU$MeanMemEstimated))

  plot(memoryProfilesS$ObjAllocations, type = "n", xlab=common_xlab, xaxt = "n", xlim = xRange, ylab=common_ylab, ylim = yRange, yaxt = "n")

  legend('topleft', c('original', 'estimate', 'measurement (with default heap size)'), 
       lty = c(1, 3, 1), bty='n', cex=.75, pch = c(19, 17, 1))

  #axis(1, x <- seq(from=1, to=length(memoryProfilesU$ObjAllocations)), labels = lapply(memoryProfilesU$ObjAllocations, csUnits))
  #axis(2, y <- seq(from=0, to=max(yRange), by=(max(yRange) - min(yRange)) / 4), labels = lapply(exp(y), csUnits))
  
  xAxis()
  yAxis()

  points(x = xTicksLog2, y = log10(memoryProfilesU$MeanMemOriginal), type="b", pch=19)
  points(x = xTicksLog2, y = log10(memoryProfilesU$MeanMemEstimated), type="b", pch=17, lty=3)
  points(x = xTicksLog2, y = log10(memoryProfilesU$MeanMemMeasurementA), type="b", pch=1)
  #points(log10(memoryProfilesU$MeanMemMeasurementB), type="b", pch=2)  
dev.off()

tableColumnNamesTotal <- c(
  "BenchmarkShortName"
  , "InputSize"
  , "ObjAllocations"
  , "MeanMemOriginal"
  , "MeanMemEstimated"
  , "MeanMemMeasurementA"                 
  , "MeanMemMeasurementB"                 
)

memoryMod17 <- read.csv("mod17-data.csv", header=FALSE, col.names = tableColumnNamesTotal)
memoryMod17ME <- read.csv("mod17-me-data.csv", header=FALSE, col.names = tableColumnNames)
memoryMod17MS <- read.csv("mod17-ms-data.csv", header=FALSE, col.names = tableColumnNames)
memoryMod17MT <- read.csv("mod17-mt-data.csv", header=FALSE, col.names = tableColumnNames)

xAt = c(5, 10, 15, 20)
xTicksLog2 = log(2^(xAt+1)-1+3, base = 2)

xAxis <- function() {
  axis(1, at = xTicksLog2, labels = paste(lapply(memoryMod17ME$ObjAllocations, csUnits), " [d=", xAt , "]", sep = ""), cex.axis=0.85)
} 

pdf("mod17.pdf", width=7, height=5)
  xRange <- range(seq(from=5, by=5, to=20)) # range(log(memoryMod17ME$ObjAllocations, base = 2))
  yRange <- range(log10(memoryMod17ME$MeanMemOriginal), log10(memoryMod17ME$MeanMemEstimated))
  
  plot(memoryMod17ME$ObjAllocations, type = "n", xlab=common_xlab, xaxt = "n", xlim = xRange, ylab=common_ylab, ylim = yRange, yaxt = "n")
  
  legend('topleft', c('original', 'estimate', 'measurement (with default heap size)'), 
         lty = c(1, 3, 1), bty='n', cex=.75, pch = c(19, 17, 1))
  
  #axis(1, x <- seq(from=1, to=length(memoryMod17ME$ObjAllocations)), labels = lapply(memoryMod17ME$ObjAllocations, csUnits))
  #axis(2, y <- seq(from=0, to=max(yRange), by=(max(yRange) - min(yRange)) / 4), labels = lapply(exp(y), csUnits))
  
  xAxis()
  yAxis()
  
  points(x = xAt, y = log10(memoryMod17ME$MeanMemOriginal), type="b", pch=19)
  points(x = xAt, y = log10(memoryMod17ME$MeanMemEstimated), type="b", pch=17, lty=3)
  #points(x = xAt, y = log10(memoryMod17ME$MeanMemMeasurementA), type="b", pch=1)
#   points(x = xAt, y = log10(memoryMod17ME$MeanMemMeasurementB), type="b", pch=2) 

  points(x = xAt, y = log10(memoryMod17MS$MeanMemOriginal), type="b", pch=19)
  points(x = xAt, y = log10(memoryMod17MS$MeanMemEstimated), type="b", pch=17, lty=3)
  #points(x = xAt, y = log10(memoryMod17MS$MeanMemMeasurementA), type="b", pch=1)
#   points(x = xAt, y = log10(memoryMod17MS$MeanMemMeasurementB), type="b", pch=2)  

#   points(x = xAt, y = log10(memoryMod17MT$MeanMemOriginal), type="b", pch=19)
#   points(x = xAt, y = log10(memoryMod17MT$MeanMemEstimated), type="b", pch=17, lty=3)
#   #points(x = xAt, y = log10(memoryMod17MT$MeanMemMeasurementA), type="b", pch=1)
#   points(x = xAt, y = log10(memoryMod17MS(memoryMod17MT$MeanMemMeasurementB), type="b", pch=2)  

dev.off()

#install.packages("ggplot2")
#install.packages("reshape2")
library(ggplot2)
library(reshape2)

#memoryMod17 <- data.frame(memoryMod17[1:2], log(memoryMod17[3:7]))

d <- ggplot(data=melt(memoryMod17[-6], id.vars=c('BenchmarkShortName','InputSize')), aes(BenchmarkShortName, y = value, fill=variable))
d <- d + geom_bar(position="dodge", stat="identity")
d

allDataColumnNames <- c(
    "BenchmarkShortName"
  , "ObjAllocations"
  , "CacheHitsEstimated"
  , "CacheFalseEquals"

  , "MemOriginal"
  , "MemEstimated"
  , "MemMeasurementA"                 
  , "MemMeasurementB" 

  , "OriginalEqualsRoot"
  , "OriginalEqualsRecursive"
  , "OriginalReferenceRoot"
  , "OriginalReferenceRecursive"
  , "OriginalEquivRoot"
  , "OriginalEquivRecursive"
  , "OriginalEqualsAndEquivRoot"
  , "OriginalEqualsAndEquivRecursive"

  , "MeasuredProgramEqualsRoot"
  , "MeasuredProgramEqualsRecursive"
  , "MeasuredProgramReferenceRoot"
  , "MeasuredProgramReferenceRecursive"
  , "MeasuredProgramEquivRoot"
  , "MeasuredProgramEquivRecursive"
  , "MeasuredProgramEqualsAndEquivRoot"
  , "MeasuredProgramEqualsAndEquivRecursive"

  , "MeasuredCacheEqualsRoot"
  , "MeasuredCacheReferenceRecursive"

  , "EstimatedCacheEqualsRoot"
  , "EstimatedCacheReferenceRecursive"
  
  , "runtimeBenchmarkA"
  , "runtimeBenchmarkB"
  , "runtimeBenchmarkAA"
  , "runtimeBenchmarkBB"
  
  , "runtimeTracrA"
  , "runtimeTracrB"
)

allData <- read.csv("all-data.csv", header=FALSE, col.names = allDataColumnNames)


###
# Dividing data in two subsets: control data set and realistic data set.
####
ctrlData <- allData[grep("^(S|U).. ", allData$BenchmarkShortName, ignore.case=T),] # excluding additional 'Min' measurements
realData <- allData[grep("^(S|U)", allData$BenchmarkShortName, ignore.case=T, invert=T),]

ctrlData_memoryMeanAccuracyFactorA <- ((ctrlData$MemEstimated) / ctrlData$MemMeasurementA)
ctrlData_memoryMeanAccuracyFactorA[is.infinite(ctrlData_memoryMeanAccuracyFactorA)] <- NA
ctrlData_memoryMeanAccuracyFactorB <- ((ctrlData$MemEstimated) / ctrlData$MemMeasurementB)
ctrlData_memoryMeanAccuracyFactorB[is.infinite(ctrlData_memoryMeanAccuracyFactorB)] <- NA

realData_memoryMeanAccuracyFactorA <- ((realData$MemEstimated) / realData$MemMeasurementA)
realData_memoryMeanAccuracyFactorA[is.infinite(realData_memoryMeanAccuracyFactorA)] <- NA
realData_memoryMeanAccuracyFactorB <- ((realData$MemEstimated) / realData$MemMeasurementB)
realData_memoryMeanAccuracyFactorB[is.infinite(realData_memoryMeanAccuracyFactorB)] <- NA


library(scales)
trans_sqrt <- trans_breaks("sqrt", function(x) x ^ 2, n=6)
trans_sqrt_sqrt <- trans_breaks(function(x) sqrt(sqrt(x)), function(x) x ^ 4, n=6)
trans_continuous <- trans_breaks("identity", function(x) x, n=6)

a_new_trans <- trans_new("sqrtsqrt", function(x) sqrt(sqrt(x)), function(x) x ^ 4)


# Plot Memory: Original vs Estimated
allDataMemSubset <- realData[,c("BenchmarkShortName", "MemOriginal", "MemEstimated")]
allDataMemSubsetLog <- data.frame(allDataMemSubset[1], log(allDataMemSubset[-1]))

pdf("viz_absolute-memory-original-vs-estimated.pdf", width=7, height=5)  
mem <- ggplot(data=melt(allDataMemSubset, id.vars=c('BenchmarkShortName')), aes(BenchmarkShortName, y = value, fill=variable))
mem <- mem + geom_bar(position="dodge", stat="identity")
mem <- mem + theme_bw()
mem <- mem + theme(legend.position="top")
mem <- mem + theme(axis.text.x = element_text(angle = 45, hjust = 1))
mem <- mem + xlab("Experiment Name") + ylab("Mean Memory Consumption")
mem <- mem + scale_fill_grey(name="Mean Memory Consumption of", # Mean Memory Consumption of
                    breaks=c("MemOriginal", "MemEstimated"),
                    labels=c("Profile", "Estimate"))
mem <- mem + scale_y_sqrt(breaks=trans_sqrt, labels=csUnitsNew_Bytes)
# mem <- mem + coord_trans(y = "sqrt")
# mem <- mem + scale_y_continuous(breaks=trans_breaks("sqrt", function(x) x ^ 2, n=10))
mem
dev.off()


# Plot Memory: Cache Hits vs Collisions (Factor)
collisionFactor <- (realData$CacheFalseEquals / realData$CacheHitsEstimated)
collisionFactor[is.infinite(collisionFactor)] <- NA

realDataCollisionFactorSubset <- data.frame(realData$BenchmarkShortName, collisionFactor)
names(realDataCollisionFactorSubset) <- c("BenchmarkShortName", "collisionFactor")
# allDataCollisionFactorSubsetLog <- data.frame(allDataCollisionFactorSubset[1], log(allDataCollisionFactorSubset[-1]))

pdf("viz_cachehits-vs-collisions.pdf", width=7, height=5)  
chf <- ggplot(data=melt(realDataCollisionFactorSubset, id.vars=c('BenchmarkShortName')), aes(BenchmarkShortName, y = value, fill=variable))
# chf <- chf + geom_boxplot() 
chf <- chf + geom_bar(position="dodge", stat="identity")
chf <- chf + theme_bw()
chf <- chf + theme(legend.position="top") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
chf <- chf + xlab("Experiment Name") + ylab("Average of False Equals per Cache Hit")
chf <- chf + scale_fill_grey(name="Average of", # Average of
                    breaks=c("collisionFactor"),
                    labels=c("False Equals per Cache Hit"))
chf <- chf + scale_y_continuous(breaks=trans_continuous, labels=csUnitsNew)
chf
dev.off()


# Plot Memory: Allocations vs Cache Hits vs Collisions 
realDataCacheHitSubset <- realData[,c("BenchmarkShortName", "ObjAllocations", "MeasuredCacheEqualsRoot", "CacheFalseEquals")]
realDataCacheHitSubsetLog <- data.frame(realDataCacheHitSubset[1], log(realDataCacheHitSubset[-1]))

pdf("viz_allocations-vs-cachehits-vs-collisions.pdf", width=7, height=5)  
ch <- ggplot(data=melt(realDataCacheHitSubset, id.vars=c('BenchmarkShortName')), aes(BenchmarkShortName, y = sqrt(value), fill=variable))
ch <- ch + geom_bar(position="dodge", stat="identity")
ch <- ch + theme_bw()
ch <- ch + theme(legend.position="top") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ch <- ch + xlab("Experiment Name") + ylab("value")
ch <- ch + scale_fill_grey(name="Measurements of",
                    breaks=c("ObjAllocations", "MeasuredCacheEqualsRoot", "CacheFalseEquals"),
                    labels=c("Object Allocations", "Cache Hits", "False Equals"))
ch <- ch + scale_y_sqrt(breaks=trans_sqrt, labels=csUnitsNew)
ch
dev.off()


# Plot Memory: Redundancy vs Mean Memory Savings
objectRedundancyFactor <- (realData$CacheHitsEstimated / realData$ObjAllocations)
memoryReductionFactor <- 1 - (realData$MemMeasurementB / realData$MemOriginal)

realDataRVSSubset <- data.frame(realData$BenchmarkShortName, objectRedundancyFactor, memoryReductionFactor)
names(realDataRVSSubset) <- c("BenchmarkShortName", "objectRedundancyFactor", "memoryReductionFactor")

pdf("viz_object-redundancy-vs-estimated-mean-memory-savings.pdf", width=7, height=5)
rvs <- ggplot(data=melt(realDataRVSSubset, id.vars=c('BenchmarkShortName')), aes(BenchmarkShortName, y = value, fill=variable))
rvs <- rvs + geom_bar(position="dodge", stat="identity")
rvs <- rvs + theme_bw()
rvs <- rvs + theme(legend.position="top") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
rvs <- rvs + xlab("Experiment Name") # + ylab("My y label")
rvs <- rvs + scale_fill_grey(name="Factor of",
                    breaks=c("objectRedundancyFactor", "memoryReductionFactor"),
                    labels=c("Object Redundancy", "Mean Memory Reduction"))
#rvs <- rvs + coord_cartesian(ylim=c(-1,1))
rvs
dev.off()
# Total Range/Mean
mean(objectRedundancyFactor)
min(objectRedundancyFactor)
max(objectRedundancyFactor)
# D--H Range/Mean
mean(realDataRVSSubset[4:8,]$objectRedundancyFactor)
min(realDataRVSSubset[4:8,]$objectRedundancyFactor)
max(realDataRVSSubset[4:8,]$objectRedundancyFactor)


# Plot Equality: Equality Profile (Root)
realDataOrigEqRootSubset <- realData[,c("BenchmarkShortName", "OriginalEqualsRoot", "OriginalReferenceRoot", "OriginalEquivRoot")]
realDataOrigEqRootSubsetLog <- data.frame(realDataOrigEqRootSubset[1], log(realDataOrigEqRootSubset[-1]))

origEqRoot <- ggplot(data=melt(realDataOrigEqRootSubset, id.vars=c('BenchmarkShortName')), aes(BenchmarkShortName, y = sqrt(sqrt(value)), fill=variable))
origEqRoot <- origEqRoot + geom_bar(position="stack", stat="identity")
origEqRoot <- origEqRoot + theme(legend.position="top") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
origEqRoot 


# Plot Equality: Equality Profile (Recursive)
realDataOrigEqRecursiveSubset <- realData[,c("BenchmarkShortName", "OriginalEqualsRecursive", "OriginalReferenceRecursive", "OriginalEquivRecursive")]
realDataOrigEqRecursiveSubsetLog <- data.frame(realDataOrigEqRecursiveSubset[1], log(realDataOrigEqRecursiveSubset[-1]))

pdf("viz_equality-profile-recursive-profiled.pdf", width=7, height=5)  
origEqRecursive <- ggplot(data=melt(realDataOrigEqRecursiveSubset, id.vars=c('BenchmarkShortName')), aes(BenchmarkShortName, y = value, fill=variable))
origEqRecursive <- origEqRecursive + geom_bar(position="stack", stat="identity")
origEqRecursive <- origEqRecursive + theme_bw()
origEqRecursive <- origEqRecursive + theme(legend.position="top") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
origEqRecursive <- origEqRecursive + xlab("Experiment Name") # + ylab("My y label")
origEqRecursive <- origEqRecursive + scale_fill_grey(name="Sum of",
                    breaks=c("OriginalEqualsRecursive", "OriginalReferenceRecursive", "OriginalEquivRecursive"),
                    labels=c("equals", " ==", "isEqual"))
origEqRecursive <- origEqRecursive + coord_cartesian(ylim=c(0,80000000))
origEqRecursive <- origEqRecursive + scale_y_continuous(breaks=trans_sqrt_sqrt, labels=csUnitsNew, trans=a_new_trans)
origEqRecursive 
dev.off()


# Plot Equality: Equality Profile (Nested)
realDataOrigEqNestedSubset <- data.frame(realData$BenchmarkShortName, realData$OriginalEqualsRecursive - realData$OriginalEqualsRoot, realData$OriginalReferenceRecursive - realData$OriginalReferenceRoot, realData$OriginalEquivRecursive - realData$OriginalEquivRoot)
names(realDataOrigEqNestedSubset) <- c("BenchmarkShortName", "OriginalEqualsNested", "OriginalReferenceNested", "OriginalEquivNested")
realDataOrigEqNestedSubsetLog <- data.frame(realDataOrigEqNestedSubset[1], log(realDataOrigEqNestedSubset[-1]))

origEqNested <- ggplot(data=melt(realDataOrigEqNestedSubset, id.vars=c('BenchmarkShortName')), aes(BenchmarkShortName, y = sqrt(sqrt(value)), fill=variable))
origEqNested <- origEqNested + geom_bar(position="dodge", stat="identity")
origEqNested <- origEqNested + theme(legend.position="top") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
origEqNested 


# Plot Equality: Root vs Recursive
equalsRootToRecFactor <- (realData$OriginalEqualsRecursive / realData$OriginalEqualsRoot)
equivRootToRecFactor <- (realData$OriginalEquivRecursive / realData$OriginalEquivRoot)

equalitiesRootToRecFactor <- ((realData$OriginalEqualsRecursive + realData$OriginalEquivRecursive) / (realData$OriginalEqualsRoot + realData$OriginalEquivRoot))
referencesRootToRecFactor <- ((realData$OriginalReferenceRecursive - realData$OriginalReferenceRoot) / (realData$OriginalEqualsRoot + realData$OriginalEquivRoot))

equalitiesRootToRecFactor[is.nan(equalitiesRootToRecFactor)] <- NA
referencesRootToRecFactor[is.infinite(referencesRootToRecFactor)] <- NA

realDataRootToRecSubset <- data.frame(realData$BenchmarkShortName, equalitiesRootToRecFactor, referencesRootToRecFactor)
names(realDataRootToRecSubset) <- c("BenchmarkShortName", "equalitiesRootToRecFactor", "referencesRootToRecFactor")

rtr <- ggplot(data=melt(realDataRootToRecSubset, id.vars=c('BenchmarkShortName')), aes(BenchmarkShortName, y = value, fill=variable))
rtr <- rtr + geom_bar(position="dodge", stat="identity")
rtr <- rtr + theme(legend.position="top") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
rtr 


# Plot Equality Measured: Equality Profile (Recursive)
realDataMeasuredEqRecursiveSubset <- realData[,c("BenchmarkShortName", "MeasuredProgramEqualsRecursive", "MeasuredProgramReferenceRecursive", "MeasuredProgramEquivRecursive")]
realDataMeasuredEqRecursiveSubsetLog <- data.frame(realDataMeasuredEqRecursiveSubset[1], log(realDataMeasuredEqRecursiveSubset[-1]))

pdf("viz_equality-profile-recursive-measured.pdf", width=7, height=5)  
measuredEqRecursive <- ggplot(data=melt(realDataMeasuredEqRecursiveSubset, id.vars=c('BenchmarkShortName')), aes(BenchmarkShortName, y = value, fill=variable))
measuredEqRecursive <- measuredEqRecursive + geom_bar(position="stack", stat="identity")
measuredEqRecursive <- measuredEqRecursive + theme_bw()
measuredEqRecursive <- measuredEqRecursive + theme(legend.position="top") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
measuredEqRecursive <- measuredEqRecursive + xlab("Experiment Name") # + ylab("My y label")
measuredEqRecursive <- measuredEqRecursive + scale_fill_grey(name="Sum of", 
                    breaks=c("MeasuredProgramEqualsRecursive", "MeasuredProgramReferenceRecursive", "MeasuredProgramEquivRecursive"),
                    labels=c("equals", "==", "isEqual"))
measuredEqRecursive <- measuredEqRecursive + coord_cartesian(ylim=c(0,80000000))
measuredEqRecursive <- measuredEqRecursive + scale_y_continuous(breaks=trans_sqrt_sqrt, labels=csUnitsNew, trans=a_new_trans)
measuredEqRecursive 
dev.off()


# Plot Equality Cache: Equality Profile (Cache)
realDataEstimatedCacheEqualsSubset <- realData[,c("BenchmarkShortName", "EstimatedCacheEqualsRoot", "EstimatedCacheReferenceRecursive", "MeasuredCacheEqualsRoot", "MeasuredCacheReferenceRecursive")]
realDataEstimatedCacheEqualsSubsetLog <- data.frame(realDataEstimatedCacheEqualsSubset[1], log(realDataEstimatedCacheEqualsSubset[-1]))

ece <- ggplot(data=melt(realDataEstimatedCacheEqualsSubset, id.vars=c('BenchmarkShortName')), aes(BenchmarkShortName, y = value, fill=variable))
ece <- ece + geom_bar(position="dodge", stat="identity")
ece <- ece + theme(legend.position="top") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ece 


# Plot Equality (Cache): Root vs Recursive
cacheReferencesRootToRecFactor <- (realData$EstimatedCacheReferenceRecursive / realData$EstimatedCacheEqualsRoot)
cacheReferencesRootToRecFactor[is.infinite(cacheReferencesRootToRecFactor)] <- NA

cacheReferencesRootToRecFactorReal <- (realData$EstimatedCacheReferenceRecursive / realData$MeasuredCacheEqualsRoot)
cacheReferencesRootToRecFactorReal[is.infinite(cacheReferencesRootToRecFactorReal)] <- NA

realDataCacheRootToRecSubset <- data.frame(realData$BenchmarkShortName, cacheReferencesRootToRecFactor, cacheReferencesRootToRecFactorReal)
names(realDataCacheRootToRecSubset) <- c("BenchmarkShortName", "cacheReferencesRootToRecFactorEstimated", "cacheReferencesRootToRecFactorReal")

crtr <- ggplot(data=melt(realDataCacheRootToRecSubset, id.vars=c('BenchmarkShortName')), aes(BenchmarkShortName, y = value, fill=variable))
crtr <- crtr + geom_bar(position="dodge", stat="identity")
crtr <- crtr + theme(legend.position="top") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
crtr 

# > mean(cacheReferencesRootToRecFactorReal)
# [1] 1.390391

# > mean(cacheReferencesRootToRecFactorEstimated)
# [1] 1.391842

# > mean(equalitiesRootToRecFactor[!is.na(equalitiesRootToRecFactor)])
# [1] 2.67706
# > mean(referencesRootToRecFactor[!is.na(referencesRootToRecFactor)])
# [1] 0.004540694


# Plot Equality (Cache): Precision
cacheEqualsAccuracyFactor <- (realData$EstimatedCacheEqualsRoot / realData$MeasuredCacheEqualsRoot)
cacheEqualsAccuracyFactor[is.infinite(cacheEqualsAccuracyFactor)] <- NA

cacheReferenceAccuracyFactor <- (realData$EstimatedCacheReferenceRecursive / realData$MeasuredCacheReferenceRecursive)
cacheReferenceAccuracyFactor[is.infinite(cacheReferenceAccuracyFactor)] <- NA

memoryMeanAccuracyFactor <- ((realData$MemEstimated) / realData$MemMeasurementB)
memoryMeanAccuracyFactor[is.infinite(memoryMeanAccuracyFactor)] <- NA

realDataEqualsAccuracySubset <- data.frame(realData$BenchmarkShortName, cacheEqualsAccuracyFactor, cacheReferenceAccuracyFactor, memoryMeanAccuracyFactor)
names(realDataEqualsAccuracySubset) <- c("BenchmarkShortName", "cacheEqualsAccuracyFactor", "cacheReferenceAccuracyFactor", "memoryMeanAccuracyFactor")

eqac <- ggplot(data=melt(realDataEqualsAccuracySubset, id.vars=c('BenchmarkShortName')), aes(BenchmarkShortName, y = value, fill=variable))
#eqac <- eqac + geom_boxplot() 
eqac <- eqac + geom_bar(position="dodge", stat="identity")
eqac <- eqac + theme(legend.position="top") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
eqac 


realDataEqualsAccuracySubset <- data.frame(realData$BenchmarkShortName, cacheEqualsAccuracyFactor)
names(realDataEqualsAccuracySubset) <- c("BenchmarkShortName", "cacheEqualsAccuracyFactor")

new <- ggplot(data=realDataEqualsAccuracySubset, aes("Overall", y = cacheEqualsAccuracyFactor))
new <- new + geom_boxplot() 
# eqac <- eqac + geom_bar(position="dodge", stat="identity")
new <- new + theme(legend.position="top") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
new 

boxplot(cacheEqualsAccuracyFactor)
boxplot(cacheReferenceAccuracyFactor)
boxplot(memoryMeanAccuracyFactor)

#With Martin
boxplot(realData$MemMeasurementA, realData$MemMeasurementB)
t.test(realData$MemEstimated, realData$MemMeasurementB)
t.test(realData$MemEstimated, realData$MemMeasurementA)

# pdf("mem-hypothesis-1-vs-2_in_control.pdf", width=7, height=5)
#   boxplot((allData$MemMeasurementA), (allData$MemMeasurementB), names=c("Method 1 (with GC-Noise)", "Method 2 (without GC-Noise)"), yaxt = "n", ylab = "Mean Memory Usage")
# 
#   #yRange <- range(allData$MemMeasurementA, allData$MemMeasurementB)
#   #axis(2, y <- seq(from=0, to=max(yRange), by=(max(yRange) - min(yRange)) / 6), labels = paste(round(y/(1024*1024), digits=0), "MB", sep = ""))
#   title("Differences in Validating Mean Memory Usage (in Control Experiment)")
# dev.off()
# t.test(allData$MemEstimated, allData$MemMeasurementB)
# t.test(allData$MemEstimated, allData$MemMeasurementA)
# 
# pdf("mem-estimate-vs-hypothesis-2_in_control.pdf", width=7, height=5)
#   boxplot(log(allData$MemEstimated), log(allData$MemMeasurementB), names=c("Orpheus Estimate", "Method 2 (without GC-Noise)"), yaxt = "n", ylab = "Mean Memory Usage")
# 
#   yRange <- range(allData$MemEstimated, allData$MemMeasurementB)
#   axis(2, y <- seq(from=0, to=max(yRange), by=(max(yRange) - min(yRange)) / 5), labels = paste(round(y/(1024*1024), digits=0), "MB", sep = ""))
#   title("Accuracy of Orpheus' Memory Estimates (in Control Experiment)")
# dev.off()

pdf("mem-hypothesis-1-vs-2_in_control.pdf", width=7, height=4)
  boxplot(ctrlData_memoryMeanAccuracyFactorA, ctrlData_memoryMeanAccuracyFactorB, names=c("Method 1 (with GC-Noise)", "Method 2 (without GC-Noise)"), yaxt = "n", ylab = "Accuracy Relative to Estimate", ylim = range(c(0, 0.2, 0.4, 0.6, 0.8, 1.0)))
  yRange <- range(ctrlData_memoryMeanAccuracyFactorA, ctrlData_memoryMeanAccuracyFactorB)
  axis(2, y <- c(0.0, 0.25, 0.5, 0.75, 1.0), labels = paste(round(y*100, digits=0), "%", sep = ""), cex.axis=0.95)
  title("Differences in Validating Mean Memory Usage (in Control Experiment)")
dev.off()
#c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)

pdf("mem-hypothesis-1-vs-2_in_real.pdf", width=7, height=3.15)
  boxplot(realData_memoryMeanAccuracyFactorA, realData_memoryMeanAccuracyFactorB, names=c("Method 1 (with GC-Noise)", "Method 2 (without GC-Noise)"), yaxt = "n", ylab = "Accuracy Relative to Estimate", ylim = range(c(0, 0.2, 0.4, 0.6, 0.8, 1.0)))
  yRange <- range(realData_memoryMeanAccuracyFactorA, realData_memoryMeanAccuracyFactorB)
  axis(2, y <- c(0.0, 0.25, 0.5, 0.75, 1.0), labels = paste(round(y*100, digits=0), "%", sep = ""), cex.axis=0.95)
  title("Differences in Validating Mean Memory Usage (in Realistic Experiment)")
  par(mar=c(0, 0, 0, 0) + 0.1)
dev.off()
#c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)

t.test(ctrlData$MemEstimated, ctrlData$MemMeasurementB)
t.test(ctrlData$MemEstimated, ctrlData$MemMeasurementA)

validation <- data.frame(allData$BenchmarkShortName, cacheEqualsAccuracyFactor)
names(validation) <- c("BenchmarkShortName", "cacheEqualsAccuracyFactor")

new <- ggplot(data=validation, aes("Overall", y = cacheEqualsAccuracyFactor))
new <- new + geom_boxplot() 
# eqac <- eqac + geom_bar(position="dodge", stat="identity")
new <- new + theme(legend.position="top") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
new 

# pdf("calibrationRedundant.pdf", width=7, height=5)  
# xRange <- range(log(memoryProfilesS$ObjAllocations, base = 2))
# yRange <- range(log10(memoryProfilesS$MeanMemOriginal), log10(memoryProfilesS$MeanMemEstimated))
# 
# plot(memoryProfilesS$ObjAllocations, type = "n", xlab=common_xlab, xaxt = "n", xlim = xRange, ylab=common_ylab, ylim = yRange, yaxt = "n")
# 
# legend('topleft', c('original', 'estimate', 'measurement (with default heap size)', 'measurement (with tight heap size)'), 
#        lty = c(1, 3, 1, 1), bty='n', cex=.75, pch = c(19, 17, 1, 13))
# 
# #xLabels = parse(text=paste("2^(", xAt, ")-1", sep=""))
# xAxis()
# yAxis()  
# 
# points(x = xTicksLog2, y = log10(memoryProfilesS$MeanMemOriginal), type="b", pch=19)
# points(x = xTicksLog2, y = log10(memoryProfilesS$MeanMemEstimated), type="b", pch=17, lty=3)
# points(x = xTicksLog2, y = log10(memoryProfilesS$MeanMemMeasurementA), type="b", pch=1)
# #points(log10(memoryProfilesS$MeanMemMeasurementB), type="b", pch=2)  
# points(x = xTicksLog2, y = log10(memoryProfilesSStressed$MeanMemMeasurementA), type="b", pch=13)
# dev.off()


## Calculate ORP vs OEP overhead.
orpVsOepDataVector <- (100 * 100 / c(96.5626,87.6487,99.9955,98.2081,99.3054,99.9315,99.8542,99.4194,96.1692,96.1707,96.1693,96.1713,96.1721,96.1728,96.1732,96.1735,96.1763,96.1737,96.1751,96.1768)) - 100
median(orpVsOepDataVector)
max(orpVsOepDataVector)