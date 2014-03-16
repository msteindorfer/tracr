#!/usr/bin/env Rscript
#setwd("~/Development/rascal-devel/tracr")
#setwd("~/Research/orpheus-results/_doTypeCheckParserGenerator-Xmx4096m-Xmx4096m")
#setwd("~/Research/orpheus-results_2014-03-06/_doExpLang-Xms4096m_-Xmx4096m-Xms4096m_-Xmx4096m")

ocNom    <- read.csv("objectCount-nom.dat", sep=" ", header=FALSE)
ocMin    <- read.csv("objectCount-min.dat", sep=" ", header=FALSE)
ocSha    <- read.csv("objectCount-sha.dat", sep=" ", header=FALSE)
ocShaMin <- read.csv("objectCount-sha-min.dat", sep=" ", header=FALSE)

hsNom     		<- read.csv("heapSizes-nom.dat", sep=" ", header=FALSE)
hsMin     		<- read.csv("heapSizes-min.dat", sep=" ", header=FALSE)
hsShaPure		  <- read.csv("heapSizes-sha.dat", sep=" ", header=FALSE)
hsShaPureMin 	<- read.csv("heapSizes-sha-min.dat", sep=" ", header=FALSE)

# maximal sharing prognosis: min + bytesPerRecordOverhead
bytesPerRecordOverhead <- 42 # TODO: solve(bytesPerRecordOverhead) to know max overhead per record to benefit
#
hsEst <- data.frame(hsMin$V1, hsMin$V2+(ocMin$V2*bytesPerRecordOverhead))
names(hsEst) <- c('V1', 'V2')
#
hsSha <- data.frame(hsShaPure$V1, hsShaPure$V2+(ocShaMin$V2*bytesPerRecordOverhead))
names(hsSha) <- c('V1', 'V2')


#diff <- (hsNom$V2-hsEst$V2)
percEst <- (hsNom$V2-hsEst$V2)*100/hsNom$V2
percSha <- (hsNom$V2-hsSha$V2)*100/hsNom$V2

# y-Axis percentage setup 
nrow <- nrow(hsNom)
xAxisPercentage <- function() {
  axis(1, x <- seq(from=0, to=nrow, by=nrow/5), labels = paste(x*100/nrow, "%", sep = ""))
}


#par(mfrow=c(3,2))

pdf("_heap-evolution.pdf")
  heapEvo_yRange <- range(hsNom$V2, hsMin$V2, hsSha$V2, hsEst$V2)
  plot(hsMin$V2, type='n', ylim=heapEvo_yRange, xaxt = "n", xlab = "Program Progress", yaxt = "n", ylab = "Memory Usage")
  #par(new=T)
  
  legend('topleft', c('measured heap size', 'simulated maximaly shared heap size'), 
       lty=1, col=c('black', 'purple'), bty='n', cex=.75)

  lines(hsNom$V2, col='black')
  #lines(hsMin$V2, col='blue', lty=3)
  lines(hsEst$V2, col='purple', lty=2)
  #lines(hsSha$V2, col='red')
  #lines(hsShaPure$V2, col='red', lty=3)
  # add a title and subtitle 
  xAxisPercentage()
  axis(2, y <- seq(from=0, to=max(heapEvo_yRange), by=(max(heapEvo_yRange) - min(heapEvo_yRange)) / 4), labels = paste(round(y/(1024*1024), digits=0), "MB", sep = ""))
  title("Heap Evolution")
dev.off()

pdf("_heap-evolution-with-validation.pdf")
  heapEvo_yRange <- range(hsNom$V2, hsMin$V2, hsSha$V2, hsEst$V2)
  plot(hsMin$V2, type='n', ylim=heapEvo_yRange, xaxt = "n", xlab = "Program Progress", yaxt = "n", ylab = "Memory Usage")
  #par(new=T)
  
  legend('topleft', c('measured', 'predicted', 'validated'), 
         lty=1, col=c('black', 'purple', 'red'), bty='n', cex=.75)
  
  lines(hsNom$V2, col='black')
  #lines(hsMin$V2, col='blue', lty=3)
  lines(hsEst$V2, col='purple', lty=2)
  lines(hsSha$V2, col='red')
  #lines(hsShaPure$V2, col='red', lty=3)
  # add a title and subtitle 
  xAxisPercentage()
  axis(2, y <- seq(from=0, to=max(heapEvo_yRange), by=(max(heapEvo_yRange) - min(heapEvo_yRange)) / 4), labels = paste(round(y/(1024*1024), digits=0), "MB", sep = ""))
  title("Heap Evolution")
dev.off()

#pdf("_memory_savings.pdf")
  # plot(diff, type='l')
  max <- range(percEst, percSha)
  range <- range(-max, max)
  plot(percEst, type='n', xlab='Program Progress', ylab='Savings (in %)', ylim=range, xaxt = "n")
  xAxisPercentage()

  legend('bottomleft', c('predicted', 'validated'), 
       lty=1, col=c('purple', 'red'), bty='n', cex=.75)

  lines(percEst, col='purple', lty=2)
  lines(percSha, col='red')
  abline(h=0)
  #lmfit <- lm(percEst ~ 1)
  #abline(lmfit)
  title("Memory Savings")
#dev.off()

###
# Equal Calls
##
eqCallsShaExt <- read.csv("equalCalls-sha-ext.dat", sep=" ", header=FALSE)
names(eqCallsShaExt) <- c('timestamp', 'rootEquals', 'recursiveEquals', 'rootReferenceEqualities',  'recursiveReferenceEqualities', 'rootLogicalEquals', 'recursiveLogicalEquals')
eqCallsShaInt <- read.csv("equalCalls-sha-int.dat", sep=" ", header=FALSE)
names(eqCallsShaInt) <- c('timestamp', 'rootEquals', 'recursiveEquals', 'rootReferenceEqualities',  'recursiveReferenceEqualities', 'rootLogicalEquals', 'recursiveLogicalEquals')

eqCallsNom <- read.csv("equalCalls-nom.dat", sep=" ", header=FALSE)
names(eqCallsNom) <- c('timestamp', 'rootEquals', 'recursiveEquals', 'rootReferenceEqualities',  'recursiveReferenceEqualities', 'rootLogicalEquals', 'recursiveLogicalEquals')

# data for all cache hits
eqCallsTmp <- read.csv("equalCalls-est.dat", sep=" ", header=FALSE)
names(eqCallsTmp) <- c('timestamp', 'rootEquals', 'recursiveReferenceEqualities')
# do estimation
eqCallsEst <- data.frame(eqCallsTmp$timestamp, 
                         eqCallsTmp$rootEquals, 
                         eqCallsTmp$recursiveReferenceEqualities + eqCallsNom$rootEquals + eqCallsNom$rootReferenceEqualities)
names(eqCallsEst) <- c('timestamp', 'rootEquals', 'recursiveReferenceEqualities')

pdf("_equality-equals-total.pdf")
  plot(cumsum(eqCallsNom$recursiveEquals), 
       ylim = range(#cumsum(eqCallsNom$recursiveEquals), cumsum(eqCallsNom$recursiveLogicalEquals),
                    cumsum(eqCallsEst$rootEquals),
                    cumsum(eqCallsShaInt$recursiveEquals)), # +eqCallsShaExt$recursiveEquals not needed
       type='n', xaxt = "n", xlab = "Program Progress", ylab = "Total of Equality Checks")
  #par(new=T)
  xAxisPercentage()

  legend('bottomright', c('program equals calls', 'estimated equals calls in sharing impl.', 'validated equals in sharing impl.'), 
       lty=1, col=c('black', 'purple', 'red'), bty='n', cex=.75)

  #lines(cumsum(eqCallsNom$recursiveEquals), type='s', col='black')
  #lines(cumsum(eqCallsNom$recursiveLogicalEquals), type='s', col='blue')
  lines(cumsum(eqCallsEst$rootEquals), type='s', col='purple', lty=2)
  #lines(cumsum(eqCallsTmp$rootEquals), type='s', col='purple', lty=2)
  lines(cumsum(eqCallsShaInt$recursiveEquals), type='s', col='red') # +eqCallsShaExt$recursiveEquals not needed
  title("Equal Calls Evolution") # Forecast (Count)
dev.off()

pdf("_equality-reference-calls-total.pdf")
  plot(cumsum(eqCallsNom$recursiveEquals), 
     ylim = range(cumsum(eqCallsEst$recursiveReferenceEqualities),
                  cumsum(eqCallsShaExt$recursiveReferenceEqualities + eqCallsShaInt$recursiveReferenceEqualities)), 
     type='n', xaxt = "n", xlab = "Program Progress", ylab = "Total of Reference Equality Checks")
  #par(new=T)
  xAxisPercentage()
  lines(cumsum(eqCallsEst$recursiveReferenceEqualities), col='purple', lty=2)
  #lines(cumsum(eqCallsTmp$recursiveReferenceEqualities), col='purple', lty=2)
  lines(cumsum(eqCallsShaExt$recursiveReferenceEqualities + eqCallsShaInt$recursiveReferenceEqualities), col='red')
  title("Reference Equality Evolution") # Forecast (Count)
dev.off()

pdf("_equality-equals-timely.pdf")
  plot(eqCallsNom$recursiveEquals, 
     ylim = range(#eqCallsNom$recursiveEquals, eqCallsNom$recursiveLogicalEquals,
                  eqCallsEst$rootEquals,
                  eqCallsShaExt$recursiveEquals + eqCallsShaInt$recursiveEquals), 
     type='n', xaxt = "n", xlab = "Program Progress", ylab = "Amount of Equality Checks")
  #par(new=T)

legend('bottomright', c('estimated', 'validated'), 
       lty=1, col=c('purple', 'red'), bty='n', cex=.75)

  xAxisPercentage()
  #lines(eqCallsNom$recursiveEquals, type='s', col='black')
  #lines(eqCallsNom$recursiveLogicalEquals, type='s', col='blue')
  lines(eqCallsEst$rootEquals, type='s', col='purple', lty=2)
  lines(eqCallsShaExt$recursiveEquals + eqCallsShaInt$recursiveEquals, type='s', col='red')
  title("Equal Calls Evolution") # Forecast (Count)
dev.off()

# #pdf("_equality-all.pdf")
#   plot(eqCallsNom$recursiveEquals, 
#        ylim = range(eqCallsNom$recursiveEquals, eqCallsEst$recursiveReferenceEqualities, eqCallsNom$recursiveLogicalEquals,
#          eqCallsEst$rootEquals,
#                     
#          eqCallsShaExt$recursiveEquals, eqCallsShaInt$recursiveEquals,
#          eqCallsShaExt$recursiveReferenceEqualities, eqCallsShaInt$recursiveReferenceEqualities
#                     ), 
#        type='n', xaxt = "n", xlab = "Program Progress", ylab = "Amount of Equality Checks")
#   #par(new=T)
#   xAxisPercentage()
#   lines(eqCallsNom$recursiveEquals, type='s', col='black')
#   lines(eqCallsNom$recursiveReference, type='s', col='red')
#   lines(eqCallsNom$recursiveLogicalEquals, type='s', col='green')
# 
#   lines(eqCallsShaInt$recursiveEquals, type='s', col='purple')
#   lines(eqCallsShaInt$recursiveReference, type='s', col='red')
#   
#   lines(eqCallsShaExt$recursiveReference, type='s', col='black')
#   lines(eqCallsShaExt$recursiveLogicalEquals, type='s', col='green')
# 
#   #lines(eqCallsEst$rootEquals, type='s', col='purple', lty=2)
#   #lines(eqCallsShaExt$recursiveEquals + eqCallsShaInt$recursiveEquals, type='s', col='red')
#   title("Equal Calls Evolution") # Forecast (Count)
# #dev.off()

# install.packages("ggplot2")
# install.packages("reshape2")
#library(ggplot2)
#library(reshape2)

#barplot(eqCallsNom)

# library(reshape2) # for melt
# 
# melted <- melt(test, "person")
# 
# melted$cat <- ''
# melted[melted$variable == 'value1',]$cat <- "first"
# melted[melted$variable != 'value1',]$cat <- "second"

pdf("_equality-reference-calls-timely.pdf")
  plot(eqCallsNom$recursiveEquals, 
       ylim = range(eqCallsEst$recursiveReferenceEqualities,
                    eqCallsShaExt$recursiveReferenceEqualities + eqCallsShaInt$recursiveReferenceEqualities), 
       type='n', xaxt = "n", xlab = "Program Progress", ylab = "Amount of Reference Equality Checks")
  #par(new=T)
  xAxisPercentage()
  lines(eqCallsEst$recursiveReferenceEqualities, col='purple', lty=2)
  #lines(eqCallsTmp$recursiveReferenceEqualities, col='purple', lty=2)
  lines(eqCallsShaExt$recursiveReferenceEqualities + eqCallsShaInt$recursiveReferenceEqualities, col='red')
  title("Reference Equality Evolution") # Forecast (Count)
dev.off()

eqPercEst <- (eqCallsNom$V7-eqCallsEst$V2)*100/eqCallsNom$V7
eqPercSha <- (eqCallsNom$V7-eqCallsShaInt$V7-eqCallsShaExt$V7)#*100/hsNom$V2
 
# barplot(eqCallsNom$V7, eqCallsEst$V2, eqCallsShaInt$V7, eqCallsShaExt$V7)

# #plot(diff, type='l')
# max <- range(eqPercEst, eqPercSha)
# range <- range(-max, max)
# plot(eqPercEst, type='l', col='purple', xlab='event counter', ylab='savings (in %)', ylim=range)
# lines(eqPercSha, col='red')
# abline(h=0, lty=3)
# #lmfit <- lm(percEst ~ 1)
# #abline(lmfit)
# title("Equal Call Savings Prognosis (in %)")


#plot(eqCallsNom$V5, ylim=range(eqCallsInt$V5,eqCallsNom$V5), type='n')
##par(new=T)
#lines(eqCallsNom$V5, col='green')
#lines(eqCallsInt$V5, col='red')
#title("Equal Calls (Time)")

#hist(eqCallsNom[,2])
#title("Equals Call Count per Deep Equals")

#hist(eqCallsNom[,3]/1000000) # in ms
#title("Equals Call Time per Deep Equals (ms)")
#dev.off()


### Create Overlap Statistic Plot
pdf("_overlap-example.pdf", width=7, height=7)
  plot(0, 0, xlim=range(0, 10), ylim=range(0,7), type = 'n', xaxt='n', xlab='Object Lifetime', ylab='Unique Object ID')
  #title("Lifetime Overlaps for Objects with Fingerprint 04DA...9A22")  

  legend('bottomright', c('measured objects', 'replacement objects'), 
       lty=c(1,6), col=c('black', 'black'), bty='n', cex=.75)

  axis(1, at = seq(0, 10, by = 1))

  xCoord1 = c(0, 3)
  yCoord1 = c(1, 1)
  lines(xCoord1, yCoord1, lwd=5)
  xCoord2 = c(2, 6)
  yCoord2 = c(2, 2)
  lines(xCoord2, yCoord2, lwd=5)
  xCoord3 = c(4, 5)
  yCoord3 = c(3, 3)
  lines(xCoord3, yCoord3, lwd=5)
  xCoord4 = c(7, 9)
  yCoord4 = c(4, 4)
  lines(xCoord4, yCoord4, lwd=5)
  xCoord5 = c(8, 10)
  yCoord5 = c(5, 5)
  lines(xCoord5, yCoord5, lwd=5)

  xCoord6 = c(0, 6)
  yCoord6 = c(6, 6)
  lines(xCoord6, yCoord6, lwd=5, col='black', lty=6)

  xCoord7 = c(7, 10)
  yCoord7 = c(7, 7)
  lines(xCoord7, yCoord7, lwd=5, col='black', lty=6)

  overlapFingerprintLabel <- '04DA...9A22'
  text((xCoord1[2] - xCoord1[1]) / 2 + xCoord1[1], yCoord1[2], overlapFingerprintLabel, pos = 1)
  text((xCoord2[2] - xCoord2[1]) / 2 + xCoord2[1], yCoord2[2], overlapFingerprintLabel, pos = 1)
  text((xCoord3[2] - xCoord3[1]) / 2 + xCoord3[1], yCoord3[2], overlapFingerprintLabel, pos = 1)
  text((xCoord4[2] - xCoord4[1]) / 2 + xCoord4[1], yCoord4[2], overlapFingerprintLabel, pos = 1)
  text((xCoord5[2] - xCoord5[1]) / 2 + xCoord5[1], yCoord5[2], overlapFingerprintLabel, pos = 1)
  text((xCoord6[2] - xCoord6[1]) / 2 + xCoord6[1], yCoord6[2], overlapFingerprintLabel, pos = 1, col = 'black')
  text((xCoord7[2] - xCoord7[1]) / 2 + xCoord7[1], yCoord7[2], overlapFingerprintLabel, pos = 1, col = 'black')

  grid(NULL, NA)
dev.off()

hsEstWith <- function(memBytesPerRecordOverhead) {
  hsTmp <- data.frame(hsMin$V1, hsMin$V2+(ocMin$V2*memBytesPerRecordOverhead))
  names(hsTmp) <- c('V1', 'V2')
  return(hsTmp)
}

hsShaWith <- function(memBytesPerRecordOverhead) {
  hsTmp <- data.frame(hsShaPure$V1, hsShaPure$V2+(ocShaMin$V2*memBytesPerRecordOverhead))
  names(hsTmp) <- c('V1', 'V2')
  return(hsTmp)
}

hsShaMinWith <- function(memBytesPerRecordOverhead) {
  hsTmp <- data.frame(hsShaPureMin$V1, hsShaPureMin$V2+(ocShaMin$V2*memBytesPerRecordOverhead))
  names(hsTmp) <- c('V1', 'V2')
  return(hsTmp)
}

hsSha <- data.frame(hsShaPure$V1, hsShaPure$V2+(ocShaMin$V2*bytesPerRecordOverhead))
names(hsSha) <- c('V1', 'V2')

savingsWithBytesPerRecordOverhead <- function(hsTmp) {
  memSavingsDevelopment <- (cumsum(as.numeric(hsNom$V2)) - cumsum(as.numeric(hsTmp$V2)))*100/cumsum(as.numeric(hsNom$V2))
  memSavings <- (sum(as.numeric(hsNom$V2)) - sum(as.numeric(hsTmp$V2)))*100/sum(as.numeric(hsNom$V2))  
  
  return(memSavings)
}

print("Summary:")

print("Equal Calls [Count] in Program [removed]")
print(sum(eqCallsNom$rootEquals))

#print("Equal Calls [Time] in Program [removed]")
#print(round(sum(eqCallsNom$V4)/1000000, 2)); print("ms") # in milli-seconds

print("Replaced by Reference Comparisions in Program [introduced]") # equals number of deep equals
print(sum(eqCallsNom$recursiveEquals))

print("Equal Calls [Count] in HashTable [introduced]")
print(sum(eqCallsShaInt$V2))

print("Average Number of Nested Equal Calls")
print(round((sum(eqCallsNom$V2) / sum(eqCallsNom$V6)) - 1, digits=2))

print("Max. # of objects at min - sha - nom")
print(max(ocMin$V2))
print(max(ocSha$V2))
print(max(ocNom$V2))


print("Expected cache hits vs measured cache hits")
cacheHitsEst <- max(cumsum(eqCallsEst$rootEquals))
cacheHitsSha <- max(cumsum(eqCallsShaInt$rootEquals))

# Cache Misses
# print(max(ocNom$V2) - max(cumsum(eqCallsEst$rootEquals)) - 2) # = object allocations - cache hits - 2 boolean constants
# print(max(ocSha$V2) - max(cumsum(eqCallsShaInt$rootEquals)) - 2) # = object allocations - cache hits - 2 boolean constants

objectsAllocated <- (max(ocNom$V1) - 1) # last timestamp only refers to garbage collection events


# print(max(cumsum(eqCallsShaExt$rootEquals)))
# print(max(cumsum(eqCallsShaExt$recursiveEquals)))
# print(max(cumsum(eqCallsNom$rootEquals)))
# print(max(cumsum(eqCallsNom$recursiveEquals)))

# print(max(cumsum(eqCallsNom$rootLogicalEquals)))
# print(max(cumsum(eqCallsShaExt$rootLogicalEquals)))
# print(max(cumsum(eqCallsNom$recursiveLogicalEquals)))
# print(max(cumsum(eqCallsShaExt$recursiveLogicalEquals)))


print("Expected reference equalities vs measured")

# eqCallsEst already contains the sum; was added before
referenceEqualitiesEst <- (
  sum(eqCallsEst$recursiveReferenceEqualities) # estimate hash-table lookups
# +sum(eqCallsNom$recursiveReferenceEqualities) # add existing reference equalities
# +sum(eqCallsNom$rootEquals) # add existing equals that are going to be replaced
)
  
referenceEqualitiesSha <- (
   sum(eqCallsShaInt$recursiveReferenceEqualities) # measured hash-table lookups
 + sum(eqCallsShaExt$recursiveReferenceEqualities) # add existing reference equalities
)
   
print(referenceEqualitiesEst)
print(referenceEqualitiesSha)


memSavingsEst0 <- savingsWithBytesPerRecordOverhead(hsEstWith(0))
memSavingsSha0 <- savingsWithBytesPerRecordOverhead(hsShaWith(0))
memSavingsShaMin0 <- savingsWithBytesPerRecordOverhead(hsShaMinWith(0))

memSavingsEst8 <- savingsWithBytesPerRecordOverhead(hsEstWith(8))
memSavingsSha8 <- savingsWithBytesPerRecordOverhead(hsShaMinWith(8))

memSavingsEst42 <- savingsWithBytesPerRecordOverhead(hsEstWith(42))
memSavingsSha42 <- savingsWithBytesPerRecordOverhead(hsShaWith(42))
memSavingsShaMin42 <- savingsWithBytesPerRecordOverhead(hsShaMinWith(42))

memSavingsEst79 <- savingsWithBytesPerRecordOverhead(hsEstWith(79))
memSavingsSha79 <- savingsWithBytesPerRecordOverhead(hsShaMinWith(79))


statFile <- "_hashAndCacheStatistic.bin.txt"
if (file.exists(statFile)) {
  hashAndCacheStatistic <- read.csv(statFile, sep=":", header=FALSE)
  statHashCollisions <- hashAndCacheStatistic$V2[1]
  statCacheHit <- hashAndCacheStatistic$V2[3]
  statCacheMiss <- hashAndCacheStatistic$V2[4] + hashAndCacheStatistic$V2[5]
} else {
  statHashCollisions <- NA
  statCacheHit <- NA
  statCacheMiss <- NA
}

##
# Writing data to disk (~appending to CSV file)
# http://stackoverflow.com/questions/8617347/how-do-i-append-a-vector-as-a-row-in-a-csv-file-with-r
# http://stackoverflow.com/questions/12381117/add-header-to-file-created-by-write-csv
###

# install.packages("sitools")
require(sitools)

# http://stackoverflow.com/questions/11340444/is-there-an-r-function-to-format-number-using-unit-prefix
f2si2<-function (number,rounding=F) 
{
  lut <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12, 1e-09, 1e-06, 
           0.001, 1, 1000, 1e+06, 1e+09, 1e+12, 1e+15, 1e+18, 1e+21, 
           1e+24)
  pre <- c("y", "z", "a", "f", "p", "n", "u", "m", "", "K", 
           "M", "G", "T", "P", "E", "Z", "Y")
  ix <- findInterval(number, lut)
  if (lut[ix]!=1) {
    if (rounding==T) {
      sistring <- paste(formatC(round(number/lut[ix]), digits = 0, format="f"), pre[ix], sep = "")
    }
    else {
      sistring <- paste(formatC(number/lut[ix], digits = 2, format="f"), pre[ix], sep = "")
    } 
  }
  else {
    sistring <- as.character(number)
  }
  return(sistring)
}

formatPercent <- function(arg) {
  if (is.nan(arg)) {
    x <- "0"
  } else {
    x <- format(round(as.numeric(arg), 0), nsmall=0, digits=2, scientific=FALSE)
  }
  
  return (paste(x, "\\%", sep = ""))
}

formatEq <- function(arg) {
  if (missing(arg) || is.na(arg) || arg == "") {
    return ("--")
  } else if (as.numeric(arg) == 0) {
    return (as.character(arg))
  } else {
    return (f2si2(as.numeric(arg), T))
  }
}

heapSize <- scan("_heapSize.bin.txt", what = '')

resultColumnNames1 <- c('O. Alloc'
                        ,'Hits Est.'
                        #,'Hits Prec.'
                        ,'Est.', 'Err.', 'Err. of min.'
#                         ,'Est. 0', 'Real. 0'
#                         ,'Est. 42', 'Real. 42'                        
                        )

features1 <- numeric(0)
features1 <- c(features1
               ,formatEq(objectsAllocated)
               
               ,formatEq(cacheHitsEst)
               #,formatPercent((cacheHitsSha - cacheHitsEst) * 100/cacheHitsEst)
                
               ,formatPercent(memSavingsEst42)  
               ,formatPercent((memSavingsSha0 - memSavingsEst0) * 100/memSavingsEst0)
               ,formatPercent((memSavingsShaMin0 - memSavingsEst0) * 100/memSavingsEst0)               
               
#                ,formatPercent(memSavingsEst0)
#                ,formatPercent((memSavingsSha0 - memSavingsEst0) * 100/memSavingsEst0)
#                
#                ,formatPercent(memSavingsEst42)
#                ,formatPercent((memSavingsSha42 - memSavingsEst42) * 100/memSavingsEst42)
)

resultColumnNamesC <- c('O. Alloc'
                       #,'Hits Est.'
                       #,'Hits Prec.'
                       ,'Est.', 'Err.', 'Err. of min.'
                       # ,'Est. 0', 'Err. 0'
                       # ,'Est. 42', 'Err. 42'
                       ,'EqEst' #,'EqEstErr'
                       ,'EqAliasEst' #,'EqAliasErr'
                       # ,'EqColl.'
                       )

featuresC <- numeric(0)
featuresC <- c(featuresC
              ,formatEq(objectsAllocated)
              
              #,formatEq(cacheHitsEst)
              #,formatPercent((cacheHitsSha - cacheHitsEst) * 100/cacheHitsEst)

              ,formatPercent(memSavingsEst42)  
              ,formatPercent((memSavingsSha0 - memSavingsEst0) * 100/memSavingsEst0)
              ,formatPercent((memSavingsShaMin0 - memSavingsEst0) * 100/memSavingsEst0)
              
              # ,formatPercent(memSavingsEst0)
              # ,formatPercent((memSavingsSha0 - memSavingsEst0) * 100/memSavingsEst0)
              
              # ,formatPercent(memSavingsEst42)
              # ,formatPercent((memSavingsSha42 - memSavingsEst42) * 100/memSavingsEst42)
                            
              ,formatEq(cacheHitsEst)
              #,formatPercent((cacheHitsSha - cacheHitsEst) * 100/cacheHitsEst)
                            
              ,formatEq(referenceEqualitiesEst)
              #,formatPercent((referenceEqualitiesSha - referenceEqualitiesEst) * 100/referenceEqualitiesEst)
              
              #,formatEq(statHashCollisions)
)

print("Expected cache hits vs measured cache hits")
equalsEst <- cacheHitsEst + sum(eqCallsNom$recursiveLogicalEquals)
equalsSha <- max(cumsum(eqCallsShaInt$rootEquals)) + max(cumsum(eqCallsShaExt$recursiveEquals))

resultColumnNames2 <- c("Allocations"
                        ,'Cache Hits'
                        ,'Mem. Model'
                        ,'Mem. Error A'
                        ,'Mem. Error B'                        
                        #
                        ,'equalsSum'
                        ,'==Sum'
                        #
                        ,'equalsEst'
                        ,'equalsSha'
                        ,'==Est'
                        ,'==Sha'
                        ,'equivEst'
                        ,'equivSha'
                        ,'Coll.'
)

features2 <- numeric(0)
features2 <- c(features2
              ,formatEq(objectsAllocated)  
              ,formatEq(cacheHitsEst)

              ,formatPercent(memSavingsEst42)  
              ,formatPercent((memSavingsSha0 - memSavingsEst0) * 100/memSavingsEst0)
              ,formatPercent((memSavingsShaMin0 - memSavingsEst0) * 100/memSavingsEst0)

              #,formatEq(sum(eqCallsNom$rootEquals))
              ,formatEq(sum(eqCallsNom$recursiveEquals))
                            
              #,formatEq(sum(eqCallsNom$rootReferenceEqualities))
              ,formatEq(sum(eqCallsNom$recursiveReferenceEqualities))
              
              #,formatEq(sum(eqCallsNom$rootLogicalEquals))
              #,formatEq(sum(eqCallsNom$recursiveLogicalEquals))
              
              # ,sum(eqCallsNom$rootEquals), sum(eqCallsNom$recursiveEquals)
              # ,sum(eqCallsNom$rootReferenceEqualities), sum(eqCallsNom$recursiveReferenceEqualities)
              # ,sum(eqCallsNom$rootLogicalEquals), sum(eqCallsNom$recursiveLogicalEquals)

              # NEW              
              #,formatEq(equalsEst)
              #,formatPercent((equalsSha - equalsEst) * 100/equalsEst)
              #
              # OLD
              ,formatEq(cacheHitsEst)
              ,formatPercent((cacheHitsSha - cacheHitsEst) * 100/cacheHitsEst)              
              
              ,formatEq(referenceEqualitiesEst)
              ,formatPercent((referenceEqualitiesSha - referenceEqualitiesEst) * 100/referenceEqualitiesEst)
              
              ,formatEq(sum(eqCallsNom$recursiveLogicalEquals))
              ,formatPercent((sum(eqCallsShaExt$recursiveLogicalEquals) - sum(eqCallsNom$recursiveLogicalEquals)) * 100/sum(eqCallsNom$recursiveLogicalEquals))
              
              ,formatEq(statHashCollisions)
)

benchmarkName <- scan("_benchmarkName.bin.txt", what = '')
benchmarkShortName <- scan("_benchmarkShortName.bin.txt", what = '')

FF1 <- as.matrix(t(features1))
rownames(FF1) <- benchmarkShortName
colnames(FF1) <- resultColumnNames1
write.table(FF1, file = "_results1.csv", sep = " & ", col.names = FALSE, append = FALSE, quote = FALSE)

FFC <- as.matrix(t(featuresC))
rownames(FFC) <- benchmarkShortName
colnames(FFC) <- resultColumnNamesC
write.table(FFC, file = "_resultsC.csv", sep = " & ", col.names = FALSE, append = FALSE, quote = FALSE)

FF2 <- as.matrix(t(features2))
rownames(FF2) <- benchmarkShortName
colnames(FF2) <- resultColumnNames2
write.table(FF2, file = "_results2.csv", sep = " & ", col.names = FALSE, append = FALSE, quote = FALSE)


### The new stuff :)
x_abcd <- data.frame(hsNom$V2, hsMin$V2, hsShaPure$V2, hsShaPureMin$V2)
names(x_abcd) <- c('hsNom', 'hsNomMin', 'hsSha', 'hsShaMin')

x_ac <- data.frame(hsNom$V2, hsShaPure$V2)
names(x_ac) <- c('hsA', 'hsB')

x_bd <- data.frame(hsMin$V2, hsShaPureMin$V2)
names(x_bd) <- c('hsA', 'hsB')

#install.packages("beanplot")
library(beanplot)

pdf("__plot_boxplot_ac.pdf")
	boxplot(x_ac)
dev.off()

pdf("__plot_boxplot_bd.pdf")
	boxplot(x_bd)
dev.off()

pdf("__plot_boxplot__all.pdf", width=7, height=5)
	boxplot(x_abcd)
dev.off()

pdf("__plot_beanplot_ac.pdf")
	beanplot(x_ac, col = c("grey", "red", "grey"), border = ("grey"))
dev.off()

pdf("__plot_beanplot_bd.pdf")
	beanplot(x_bd, col = c("grey", "red", "grey"), border = ("grey"))
dev.off()

pdf("__plot_beanplot__all.pdf")
	beanplot(x_abcd, col = c("grey", "red", "grey"), border = ("grey"))
dev.off()


#####
#
ocNomWithoutReordering <- read.csv("objectCount-nom-without-reordering.dat", sep=" ", header=FALSE)
ocMinWithoutReordering <- read.csv("objectCount-min-without-reordering.dat", sep=" ", header=FALSE)
hsNomWithoutReordering <- read.csv("heapSizes-nom-without-reordering.dat", sep=" ", header=FALSE)
hsMinWithoutReordering <- read.csv("heapSizes-min-without-reordering.dat", sep=" ", header=FALSE)
hsEstWithoutReordering <- data.frame(hsMinWithoutReordering$V1, hsMinWithoutReordering$V2+(ocMinWithoutReordering$V2*bytesPerRecordOverhead))
names(hsEstWithoutReordering) <- c('V1', 'V2')

x_ord_vs_unord <- data.frame(hsNom$V2, hsMin$V2, hsNomWithoutReordering$V2, hsMinWithoutReordering$V2)
names(x_ord_vs_unord) <- c('hsNom', 'hsNomMin', 'hsNomWithoutReordering', 'hsNomMinWithoutReordering')

ocNomWithXORHashing <- read.csv("objectCount-nom-with-xor-hashing.dat", sep=" ", header=FALSE)
ocMinWithXORHashing <- read.csv("objectCount-min-with-xor-hashing.dat", sep=" ", header=FALSE)
hsNomWithXORHashing <- read.csv("heapSizes-nom-with-xor-hashing.dat", sep=" ", header=FALSE)
hsMinWithXORHashing <- read.csv("heapSizes-min-with-xor-hashing.dat", sep=" ", header=FALSE)
hsEstWithXORHashing <- data.frame(hsMinWithXORHashing$V1, hsMinWithXORHashing$V2+(ocMinWithXORHashing$V2*bytesPerRecordOverhead))
names(hsEstWithXORHashing) <- c('V1', 'V2')

pdf("__plot_boxplot__ord_vs_unord.pdf")
  boxplot(x_ord_vs_unord)
dev.off()

pdf("__plot_beanplot__ord_vs_unord.pdf")
  beanplot(x_ord_vs_unord, col = c("grey", "red", "grey"), border = ("grey"))
dev.off()

plot(x_ord_vs_unord$hsNom, type='n')
lines(x_ord_vs_unord$hsNom, col='black')
lines(x_ord_vs_unord$hsNomWithoutReordering, col='purple')

plot(hsNom$V2, type='n')
lines(hsNom$V2, col='black')
lines(hsNomWithoutReordering$V2, col='purple')
lines(hsNomWithXORHashing$V2, col='red')

plot(hsMin$V2, type='n')
lines(hsMin$V2, col='black')
lines(hsMinWithoutReordering$V2, col='purple')
lines(hsMinWithXORHashing$V2, col='red')
# lines(hsMinWithoutReordering$V2, col='red')
# lines(hsEst$V2, col='purple')
# lines(hsEstWithoutReordering$V2, col='purple')

plot(ocNom$V2, type='n')
lines(ocNom$V2, col='black')
lines(ocNomWithoutReordering$V2, col='purple')
lines(ocNomWithXORHashing$V2, col='red')

plot(ocMin$V2, type='n')
lines(ocMin$V2, col='black')
lines(ocMinWithoutReordering$V2, col='purple')
lines(ocMinWithXORHashing$V2, col='red')
#
####



lm_nom <- lm(x_ac)
lm_min <- lm(x_bd)
