setwd("~/Development/rascal-devel/tracr")

hsNom <- read.csv("heapSizes-nom.dat", sep=" ", header=FALSE)
hsMin <- read.csv("heapSizes-min.dat", sep=" ", header=FALSE)

diff <- (hsNom$V2-hsMin$V2)
perc <- (hsNom$V2-hsMin$V2)*100/hsNom$V2

par(mfrow=c(2,2))
plot(perc, type='h', col='red', xlab='event counter', ylab='savings (in %)')
title("Best Case Memory Savings (in %)")

plot(diff, type='l')


plot(hsNom, type='n')
#par(new=T)
lines(hsNom, col=sample(rainbow(10)))
lines(hsMin, col=sample(rainbow(10)))

# add a title and subtitle 
title("Heap Evolution") 


###
# Equal Calls
##
#eqCalls <- read.csv("equalCalls.dat", sep=" ", header=FALSE)

#barplot(eqCalls[,2])
#title("Equals Call Count per Deep Equals")

#barplot(eqCalls[,3]/1000000) # in ms
#title("Equals Call Time per Deep Equals (ms)")
#barplot(eqCalls$V1, eqCalls$V2, beside = TRUE)
#barplot(eqCalls$V1, eqCalls$V3, beside = TRUE)
  