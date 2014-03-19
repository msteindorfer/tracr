#!/usr/bin/env Rscript

memoryProfilesS <- read.csv("calibration-redundant-data.csv", header=FALSE)

plot(memoryProfilesS$V1, type = "n", ylim = range(log(memoryProfilesS$V2), log(memoryProfilesS$V3)))
lines(log(memoryProfilesS$V2))
lines(log(memoryProfilesS$V3))
lines(log(memoryProfilesS$V4))
lines(log(memoryProfilesS$V5))

memoryProfilesU <- read.csv("calibration-redundancy-free-data.csv", header=FALSE)

plot(memoryProfilesU$V1, type = "n", ylim = range(log(memoryProfilesU$V2), log(memoryProfilesU$V3)))
lines(log(memoryProfilesU$V2))
lines(log(memoryProfilesU$V3))
lines(log(memoryProfilesU$V4))
lines(log(memoryProfilesU$V5))