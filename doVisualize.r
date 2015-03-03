#!/usr/bin/env Rscript
setwd("~/Development/rascal-devel/tracr")

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

# Plot Memory: Original vs Estimated
allDataMemSubset <- allData[,c("BenchmarkShortName", "MemOriginal", "MemEstimated")]
allDataMemSubsetLog <- data.frame(allDataMemSubset[1], log(allDataMemSubset[-1]))

pdf("viz_absolute-memory-original-vs-estimated.pdf", width=7, height=5)  
mem <- ggplot(data=melt(allDataMemSubset, id.vars=c('BenchmarkShortName')), aes(BenchmarkShortName, y = sqrt(value), fill=variable))
mem <- mem + geom_bar(position="dodge", stat="identity")
mem <- mem + theme_bw()
mem <- mem + theme(legend.position="top")
mem <- mem + theme(axis.text.x = element_text(angle = 45, hjust = 1))
mem <- mem + xlab("Experiment Name") # + ylab("My y label")
mem <- mem + scale_fill_grey(name="Mean Memory Consumption of",
                    breaks=c("MemOriginal", "MemEstimated"),
                    labels=c("Profile", "Estimate"))
mem
dev.off()


# Plot Memory: Cache Hits vs Collisions (Factor)
collisionFactor <- (allData$CacheFalseEquals / allData$CacheHitsEstimated)
collisionFactor[is.infinite(collisionFactor)] <- NA

allDataCollisionFactorSubset <- data.frame(allData$BenchmarkShortName, collisionFactor)
names(allDataCollisionFactorSubset) <- c("BenchmarkShortName", "collisionFactor")
# allDataCollisionFactorSubsetLog <- data.frame(allDataCollisionFactorSubset[1], log(allDataCollisionFactorSubset[-1]))

pdf("viz_cachehits-vs-collisions.pdf", width=7, height=5)  
chf <- ggplot(data=melt(allDataCollisionFactorSubset, id.vars=c('BenchmarkShortName')), aes(BenchmarkShortName, y = value, fill=variable))
# chf <- chf + geom_boxplot() 
chf <- chf + geom_bar(position="dodge", stat="identity")
chf <- chf + theme_bw()
chf <- chf + theme(legend.position="top") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
chf <- chf + xlab("Experiment Name") # + ylab("My y label")
chf <- chf + scale_fill_grey(name="Average of",
                    breaks=c("collisionFactor"),
                    labels=c("False Equals per Cache Hit"))
chf
dev.off()


# Plot Memory: Allocations vs Cache Hits vs Collisions 
allDataCacheHitSubset <- allData[,c("BenchmarkShortName", "ObjAllocations", "MeasuredCacheEqualsRoot", "CacheFalseEquals")]
allDataCacheHitSubsetLog <- data.frame(allDataCacheHitSubset[1], log(allDataCacheHitSubset[-1]))

pdf("viz_allocations-vs-cachehits-vs-collisions.pdf", width=7, height=5)  
ch <- ggplot(data=melt(allDataCacheHitSubset, id.vars=c('BenchmarkShortName')), aes(BenchmarkShortName, y = sqrt(value), fill=variable))
ch <- ch + geom_bar(position="dodge", stat="identity")
ch <- ch + theme_bw()
ch <- ch + theme(legend.position="top") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ch <- ch + xlab("Experiment Name") # + ylab("My y label")
ch <- ch + scale_fill_grey(name="Measurments of",
                    breaks=c("ObjAllocations", "MeasuredCacheEqualsRoot", "CacheFalseEquals"),
                    labels=c("Object Allocations", "Cache Hits", "False Equals"))
ch
dev.off()


# Plot Memory: Redundancy vs Mean Memory Savings
objectRedundancyFactor <- (allData$CacheHitsEstimated / allData$ObjAllocations)
memoryReductionFactor <- 1 - (allData$MemMeasurementB / allData$MemOriginal)

allDataRVSSubset <- data.frame(allData$BenchmarkShortName, objectRedundancyFactor, memoryReductionFactor)
names(allDataRVSSubset) <- c("BenchmarkShortName", "objectRedundancyFactor", "memoryReductionFactor")

pdf("viz_object-redundancy-vs-estimated-mean-memory-savings.pdf", width=7, height=5)
rvs <- ggplot(data=melt(allDataRVSSubset, id.vars=c('BenchmarkShortName')), aes(BenchmarkShortName, y = value, fill=variable))
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
mean(allDataRVSSubset[4:8,]$objectRedundancyFactor)
min(allDataRVSSubset[4:8,]$objectRedundancyFactor)
max(allDataRVSSubset[4:8,]$objectRedundancyFactor)


# Plot Equality: Equality Profile (Root)
allDataOrigEqRootSubset <- allData[,c("BenchmarkShortName", "OriginalEqualsRoot", "OriginalReferenceRoot", "OriginalEquivRoot")]
allDataOrigEqRootSubsetLog <- data.frame(allDataOrigEqRootSubset[1], log(allDataOrigEqRootSubset[-1]))

origEqRoot <- ggplot(data=melt(allDataOrigEqRootSubset, id.vars=c('BenchmarkShortName')), aes(BenchmarkShortName, y = sqrt(sqrt(value)), fill=variable))
origEqRoot <- origEqRoot + geom_bar(position="stack", stat="identity")
origEqRoot <- origEqRoot + theme(legend.position="top") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
origEqRoot 


# Plot Equality: Equality Profile (Recursive)
allDataOrigEqRecursiveSubset <- allData[,c("BenchmarkShortName", "OriginalEqualsRecursive", "OriginalReferenceRecursive", "OriginalEquivRecursive")]
allDataOrigEqRecursiveSubsetLog <- data.frame(allDataOrigEqRecursiveSubset[1], log(allDataOrigEqRecursiveSubset[-1]))

pdf("viz_equality-profile-recursive-profiled.pdf", width=7, height=5)  
origEqRecursive <- ggplot(data=melt(allDataOrigEqRecursiveSubset, id.vars=c('BenchmarkShortName')), aes(BenchmarkShortName, y = sqrt(sqrt(value)), fill=variable))
origEqRecursive <- origEqRecursive + geom_bar(position="stack", stat="identity")
origEqRecursive <- origEqRecursive + theme_bw()
origEqRecursive <- origEqRecursive + theme(legend.position="top") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
origEqRecursive <- origEqRecursive + xlab("Experiment Name") # + ylab("My y label")
origEqRecursive <- origEqRecursive + scale_fill_grey(name="Sum of",
                    breaks=c("OriginalEqualsRecursive", "OriginalReferenceRecursive", "OriginalEquivRecursive"),
                    labels=c("equals", " ==", "isEqual"))
origEqRecursive <- origEqRecursive + coord_cartesian(ylim=c(0,100))
origEqRecursive 
dev.off()


# Plot Equality: Equality Profile (Nested)
allDataOrigEqNestedSubset <- data.frame(allData$BenchmarkShortName, allData$OriginalEqualsRecursive - allData$OriginalEqualsRoot, allData$OriginalReferenceRecursive - allData$OriginalReferenceRoot, allData$OriginalEquivRecursive - allData$OriginalEquivRoot)
names(allDataOrigEqNestedSubset) <- c("BenchmarkShortName", "OriginalEqualsNested", "OriginalReferenceNested", "OriginalEquivNested")
allDataOrigEqNestedSubsetLog <- data.frame(allDataOrigEqNestedSubset[1], log(allDataOrigEqNestedSubset[-1]))

origEqNested <- ggplot(data=melt(allDataOrigEqNestedSubset, id.vars=c('BenchmarkShortName')), aes(BenchmarkShortName, y = sqrt(sqrt(value)), fill=variable))
origEqNested <- origEqNested + geom_bar(position="dodge", stat="identity")
origEqNested <- origEqNested + theme(legend.position="top") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
origEqNested 


# Plot Equality: Root vs Recursive
equalsRootToRecFactor <- (allData$OriginalEqualsRecursive / allData$OriginalEqualsRoot)
equivRootToRecFactor <- (allData$OriginalEquivRecursive / allData$OriginalEquivRoot)

equalitiesRootToRecFactor <- ((allData$OriginalEqualsRecursive + allData$OriginalEquivRecursive) / (allData$OriginalEqualsRoot + allData$OriginalEquivRoot))
referencesRootToRecFactor <- ((allData$OriginalReferenceRecursive - allData$OriginalReferenceRoot) / (allData$OriginalEqualsRoot + allData$OriginalEquivRoot))

equalitiesRootToRecFactor[is.nan(equalitiesRootToRecFactor)] <- NA
referencesRootToRecFactor[is.infinite(referencesRootToRecFactor)] <- NA

allDataRootToRecSubset <- data.frame(allData$BenchmarkShortName, equalitiesRootToRecFactor, referencesRootToRecFactor)
names(allDataRootToRecSubset) <- c("BenchmarkShortName", "equalitiesRootToRecFactor", "referencesRootToRecFactor")

rtr <- ggplot(data=melt(allDataRootToRecSubset, id.vars=c('BenchmarkShortName')), aes(BenchmarkShortName, y = value, fill=variable))
rtr <- rtr + geom_bar(position="dodge", stat="identity")
rtr <- rtr + theme(legend.position="top") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
rtr 


# Plot Equality Measured: Equality Profile (Recursive)
allDataMeasuredEqRecursiveSubset <- allData[,c("BenchmarkShortName", "MeasuredProgramEqualsRecursive", "MeasuredProgramReferenceRecursive", "MeasuredProgramEquivRecursive")]
allDataMeasuredEqRecursiveSubsetLog <- data.frame(allDataMeasuredEqRecursiveSubset[1], log(allDataMeasuredEqRecursiveSubset[-1]))

pdf("viz_equality-profile-recursive-measured.pdf", width=7, height=5)  
measuredEqRecursive <- ggplot(data=melt(allDataMeasuredEqRecursiveSubset, id.vars=c('BenchmarkShortName')), aes(BenchmarkShortName, y = sqrt(sqrt(value)), fill=variable))
measuredEqRecursive <- measuredEqRecursive + geom_bar(position="stack", stat="identity")
measuredEqRecursive <- measuredEqRecursive + theme_bw()
measuredEqRecursive <- measuredEqRecursive + theme(legend.position="top") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
measuredEqRecursive <- measuredEqRecursive + xlab("Experiment Name") # + ylab("My y label")
measuredEqRecursive <- measuredEqRecursive + scale_fill_grey(name="Sum of", 
                    breaks=c("MeasuredProgramEqualsRecursive", "MeasuredProgramReferenceRecursive", "MeasuredProgramEquivRecursive"),
                    labels=c("equals", "==", "isEqual"))
measuredEqRecursive <- measuredEqRecursive + coord_cartesian(ylim=c(0,100))
measuredEqRecursive 
dev.off()


# Plot Equality Cache: Equality Profile (Cache)
allDataEstimatedCacheEqualsSubset <- allData[,c("BenchmarkShortName", "EstimatedCacheEqualsRoot", "EstimatedCacheReferenceRecursive", "MeasuredCacheEqualsRoot", "MeasuredCacheReferenceRecursive")]
allDataEstimatedCacheEqualsSubsetLog <- data.frame(allDataEstimatedCacheEqualsSubset[1], log(allDataEstimatedCacheEqualsSubset[-1]))

ece <- ggplot(data=melt(allDataEstimatedCacheEqualsSubset, id.vars=c('BenchmarkShortName')), aes(BenchmarkShortName, y = value, fill=variable))
ece <- ece + geom_bar(position="dodge", stat="identity")
ece <- ece + theme(legend.position="top") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ece 


# Plot Equality (Cache): Root vs Recursive
cacheReferencesRootToRecFactor <- (allData$EstimatedCacheReferenceRecursive / allData$EstimatedCacheEqualsRoot)
cacheReferencesRootToRecFactor[is.infinite(cacheReferencesRootToRecFactor)] <- NA

cacheReferencesRootToRecFactorReal <- (allData$EstimatedCacheReferenceRecursive / allData$MeasuredCacheEqualsRoot)
cacheReferencesRootToRecFactorReal[is.infinite(cacheReferencesRootToRecFactorReal)] <- NA

allDataCacheRootToRecSubset <- data.frame(allData$BenchmarkShortName, cacheReferencesRootToRecFactor, cacheReferencesRootToRecFactorReal)
names(allDataCacheRootToRecSubset) <- c("BenchmarkShortName", "cacheReferencesRootToRecFactorEstimated", "cacheReferencesRootToRecFactorReal")

crtr <- ggplot(data=melt(allDataCacheRootToRecSubset, id.vars=c('BenchmarkShortName')), aes(BenchmarkShortName, y = value, fill=variable))
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
cacheEqualsAccuracyFactor <- (allData$EstimatedCacheEqualsRoot / allData$MeasuredCacheEqualsRoot)
cacheEqualsAccuracyFactor[is.infinite(cacheEqualsAccuracyFactor)] <- NA

cacheReferenceAccuracyFactor <- (allData$EstimatedCacheReferenceRecursive / allData$MeasuredCacheReferenceRecursive)
cacheReferenceAccuracyFactor[is.infinite(cacheReferenceAccuracyFactor)] <- NA

memoryMeanAccuracyFactor <- ((allData$MemEstimated) / allData$MemMeasurementB)
memoryMeanAccuracyFactor[is.infinite(memoryMeanAccuracyFactor)] <- NA

allDataEqualsAccuracySubset <- data.frame(allData$BenchmarkShortName, cacheEqualsAccuracyFactor, cacheReferenceAccuracyFactor, memoryMeanAccuracyFactor)
names(allDataEqualsAccuracySubset) <- c("BenchmarkShortName", "cacheEqualsAccuracyFactor", "cacheReferenceAccuracyFactor", "memoryMeanAccuracyFactor")

eqac <- ggplot(data=melt(allDataEqualsAccuracySubset, id.vars=c('BenchmarkShortName')), aes(BenchmarkShortName, y = value, fill=variable))
#eqac <- eqac + geom_boxplot() 
eqac <- eqac + geom_bar(position="dodge", stat="identity")
eqac <- eqac + theme(legend.position="top") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
eqac 


allDataEqualsAccuracySubset <- data.frame(allData$BenchmarkShortName, cacheEqualsAccuracyFactor)
names(allDataEqualsAccuracySubset) <- c("BenchmarkShortName", "cacheEqualsAccuracyFactor")

new <- ggplot(data=allDataEqualsAccuracySubset, aes("Overall", y = cacheEqualsAccuracyFactor))
new <- new + geom_boxplot() 
# eqac <- eqac + geom_bar(position="dodge", stat="identity")
new <- new + theme(legend.position="top") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
new 

boxplot(cacheEqualsAccuracyFactor)
boxplot(cacheReferenceAccuracyFactor)
boxplot(memoryMeanAccuracyFactor)

#With Martin
boxplot(allData$MemMeasurementA, allData$MemMeasurementB)
t.test(allData$MemEstimated, allData$MemMeasurementB)
t.test(allData$MemEstimated, allData$MemMeasurementA)

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

pdf("mem-hypothesis-1-vs-2_in_real.pdf", width=7, height=4)
  boxplot(realData_memoryMeanAccuracyFactorA, realData_memoryMeanAccuracyFactorB, names=c("Method 1 (with GC-Noise)", "Method 2 (without GC-Noise)"), yaxt = "n", ylab = "Accuracy Relative to Estimate", ylim = range(c(0, 0.2, 0.4, 0.6, 0.8, 1.0)))
  yRange <- range(realData_memoryMeanAccuracyFactorA, realData_memoryMeanAccuracyFactorB)
  axis(2, y <- c(0.0, 0.25, 0.5, 0.75, 1.0), labels = paste(round(y*100, digits=0), "%", sep = ""), cex.axis=0.95)
  title("Differences in Validating Mean Memory Usage (in Realistic Experiment)")
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
