setwd("~/Development/rascal-devel/tracr")

ocNom <- read.csv("objectCount-nom.dat", sep=" ", header=FALSE)
ocMin <- read.csv("objectCount-min.dat", sep=" ", header=FALSE)

hsNom <- read.csv("heapSizes-nom.dat", sep=" ", header=FALSE)
hsMin <- read.csv("heapSizes-min.dat", sep=" ", header=FALSE)
# hsAvg <- c(hsMin$V1, hsMin$V2+(ocMin$V2*8)) # maximal sharing prognosis: min + 8 byte per hashtable record
hsAvg <- data.frame(hsMin$V1, hsMin$V2+(ocMin$V2*8)) # maximal sharing prognosis: min + 8 byte per hashtable record
names(hsAvg) <- c('V1', 'V2')

diff <- (hsNom$V2-hsAvg$V2)
perc <- (hsNom$V2-hsAvg$V2)*100/hsNom$V2

#pdf("r-graph.pdf")
par(mfrow=c(3,1))

plot(hsMin, type='n', ylim=range(hsNom$V2, hsMin$V2, hsAvg$V2))
#par(new=T)
lines(hsNom, col='green')
lines(hsMin, col='blue', lty=3)
lines(hsAvg, col='purple', lty=2)
# add a title and subtitle 
title("Heap Evolution")

#plot(diff, type='l')
max <- range(perc)
range <- range(-max, max)
plot(perc, type='l', col='red', xlab='event counter', ylab='savings (in %)', ylim=range)
abline(h=0, lty=3)
#lmfit <- lm(perc ~ 1)
#abline(lmfit)
title("Memory Savings Prognosis (in %)")

###
# Equal Calls
##
eqCallsExt <- read.csv("equalCalls-ext.dat", sep=" ", header=FALSE)
eqCallsInt <- read.csv("equalCalls-int.dat", sep=" ", header=FALSE)

eqCallsTmp <- read.csv("equalCalls-est.dat", sep=" ", header=FALSE)
eqCallsEst <- data.frame(eqCallsTmp$V1, cumsum(eqCallsTmp$V2))
names(eqCallsEst) <- c('V1', 'V2')

plot(eqCallsExt$V3, ylim=range(eqCallsInt$V3,eqCallsExt$V3,eqCallsEst$V2), type='n')
#par(new=T)
lines(eqCallsExt$V3, col='green')                 # = ProgramEquals
lines(eqCallsInt$V3, col='red')                   # = UniverseHashTableEquals (+MaxSharingForecastEquals?)
lines(eqCallsEst$V2, col='purple', lty=2)
lines(eqCallsExt$V7, col='blue', lty=3)           # = MinAmount
title("Equal Calls (Count)")

#plot(eqCallsExt$V5, ylim=range(eqCallsInt$V5,eqCallsExt$V5), type='n')
##par(new=T)
#lines(eqCallsExt$V5, col='green')
#lines(eqCallsInt$V5, col='red')
#title("Equal Calls (Time)")

#hist(eqCallsExt[,2])
#title("Equals Call Count per Deep Equals")

#hist(eqCallsExt[,3]/1000000) # in ms
#title("Equals Call Time per Deep Equals (ms)")
#dev.off()
