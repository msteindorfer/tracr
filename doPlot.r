setwd("~/Development/rascal-devel/tracr")

ocNom    <- read.csv("objectCount-nom.dat", sep=" ", header=FALSE)
ocMin    <- read.csv("objectCount-min.dat", sep=" ", header=FALSE)
ocSha    <- read.csv("objectCount-sha.dat", sep=" ", header=FALSE)
ocShaMin <- read.csv("objectCount-sha-min.dat", sep=" ", header=FALSE)

hsNom     <- read.csv("heapSizes-nom.dat", sep=" ", header=FALSE)
hsMin     <- read.csv("heapSizes-min.dat", sep=" ", header=FALSE)
hsShaPure <- read.csv("heapSizes-sha.dat", sep=" ", header=FALSE)
# maximal sharing prognosis: min + bytesPerRecordOverhead
bytesPerRecordOverhead <- 42
#
hsEst <- data.frame(hsMin$V1, hsMin$V2+(ocMin$V2*bytesPerRecordOverhead))
names(hsEst) <- c('V1', 'V2')
#
hsSha <- data.frame(hsShaPure$V1, hsShaPure$V2+(ocShaMin$V2*bytesPerRecordOverhead))
names(hsSha) <- c('V1', 'V2')

hashTableSizeSha <- read.csv("hashTableSize-sha.dat", sep=" ", header=FALSE)

meanOverheadPerEntry <- mean((hashTableSizeSha/ocMin)$V2)

#diff <- (hsNom$V2-hsEst$V2)
percEst <- (hsNom$V2-hsEst$V2)*100/hsNom$V2
percSha <- (hsNom$V2-hsSha$V2)*100/hsNom$V2

# y-Axis percentage setup 
xAxisPercentage <- function() {
  axis(1, x <- seq(from=0, to=3000, by=600), labels = paste(x*100/3000, "%", sep = ""))
}


par(mfrow=c(2,1))

pdf("_heap-evolution.pdf")
  heapEvo_yRange <- range(hsNom$V2, hsMin$V2, hsSha$V2, hsEst$V2)
  plot(hsMin$V2, type='n', ylim=heapEvo_yRange, xaxt = "n", xlab = "Program Progress", yaxt = "n", ylab = "Memory Usage")
  #par(new=T)
  
  lines(hsNom$V2, col='green')
  #lines(hsMin$V2, col='blue', lty=3)
  lines(hsEst$V2, col='purple', lty=2)
  #lines(hsSha$V2, col='red') # TODO: add hashTableSizeSha
  # add a title and subtitle 
  xAxisPercentage()
  axis(2, y <- seq(from=0, to=max(heapEvo_yRange), by=(max(heapEvo_yRange) - min(heapEvo_yRange)) / 4), labels = paste(round(y/(1024*1024), digits=0), "MB", sep = ""))
  title("Heap Evolution")
dev.off()


# plot(diff, type='l')
max <- range(percEst, percSha)
range <- range(-max, max)
plot(percEst, type='n', xlab='event counter', ylab='savings (in %)', ylim=range, xaxt = "n")
xAxisPercentage()
lines(percEst, col='purple', lty=2)
lines(percSha, col='red')
abline(h=0)
#lmfit <- lm(percEst ~ 1)
#abline(lmfit)
title("Memory Savings Prognosis (in %)")

###
# Equal Calls
##
eqCallsShaExt <- read.csv("equalCalls-sha-ext.dat", sep=" ", header=FALSE)
eqCallsShaInt <- read.csv("equalCalls-sha-int.dat", sep=" ", header=FALSE)

eqCallsNom <- read.csv("equalCalls-nom.dat", sep=" ", header=FALSE)
eqCallsTmp <- read.csv("equalCalls-est.dat", sep=" ", header=FALSE)
eqCallsEst <- data.frame(eqCallsTmp$V1, cumsum(eqCallsTmp$V2) + eqCallsNom$V7)
names(eqCallsEst) <- c('V1', 'V2')

pdf("_equal-calls.pdf")
  plot(eqCallsNom$V3, ylim=range(eqCallsNom$V3,eqCallsEst$V2,eqCallsShaExt$V3+eqCallsShaInt$V3), type='n', xaxt = "n", xlab = "Program Progress", ylab = "Amount of Equality Checks")
  #par(new=T)
  xAxisPercentage()
  lines(eqCallsNom$V3, col='green')                 # = ProgramEquals
  lines(eqCallsEst$V2, col='purple', lty=2)
  lines(eqCallsNom$V7, col='blue', lty=3)           # = MinAmount
  
  #lines(eqCallsShaInt$V3, col='red', lty=3)
  #lines(eqCallsShaExt$V3, col='red', lty=3)
  lines(eqCallsShaExt$V3+eqCallsShaInt$V3, col='red')
  title("Equal Calls Evolution") # Forecast (Count)
dev.off()

#pdf("_equal-calls-maximal-sharing.pdf")
  plot(eqCallsNom$V3, ylim=range(eqCallsShaExt$V3,eqCallsShaInt$V3), type='n', xaxt = "n")
  #par(new=T)
  xAxisPercentage()
  lines(eqCallsShaInt$V3, col='red', lty=3)
  lines(eqCallsShaExt$V3, col='red', lty=3)
  #lines(eqCallsShaExt$V3+eqCallsShaInt$V3, col='red', lty=3)
  title("Equals in Maximal Sharing (Count)")
#dev.off()

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
pdf("_overlap-example.pdf")
  plot(0, 0, xlim=range(0, 10), ylim=range(0,7), type = 'n', xaxt='n', xlab='Object Lifetime', ylab='Unique Object ID')
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
  lines(xCoord6, yCoord6, lwd=5, col='red')

  xCoord7 = c(7, 10)
  yCoord7 = c(7, 7)
  lines(xCoord7, yCoord7, lwd=5, col='red')

  overlapFingerprintLabel <- '04DA...9A22'
  text((xCoord1[2] - xCoord1[1]) / 2 + xCoord1[1], yCoord1[2], overlapFingerprintLabel, pos = 1)
  text((xCoord2[2] - xCoord2[1]) / 2 + xCoord2[1], yCoord2[2], overlapFingerprintLabel, pos = 1)
  text((xCoord3[2] - xCoord3[1]) / 2 + xCoord3[1], yCoord3[2], overlapFingerprintLabel, pos = 1)
  text((xCoord4[2] - xCoord4[1]) / 2 + xCoord4[1], yCoord4[2], overlapFingerprintLabel, pos = 1)
  text((xCoord5[2] - xCoord5[1]) / 2 + xCoord5[1], yCoord5[2], overlapFingerprintLabel, pos = 1)
  text((xCoord6[2] - xCoord6[1]) / 2 + xCoord6[1], yCoord6[2], overlapFingerprintLabel, pos = 1, col = 'red')
  text((xCoord7[2] - xCoord7[1]) / 2 + xCoord7[1], yCoord7[2], overlapFingerprintLabel, pos = 1, col = 'red')

  grid(NULL, NA)
dev.off()