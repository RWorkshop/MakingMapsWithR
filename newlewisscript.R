setwd("C:/Users/Kevin/Documents/GitHub/MCS-Chap1")
lewis <- read.csv("LEWISDATA.csv",header=T)
lewis <- lewis[1:120,]

newlewis <- rbind(lewis,lewis,lewis)
dim(newlewis)

newlewis = c(newlewis[,1],newlewis[,2])
length(newlewis)
set.seed(12324)
newlewis <- data.frame(y =jitter(newlewis,factor=1)
,item = rep(1:120,6),
   repl = c(rep(1:3,each=120),rep(1:3,each=120)),
   meth=rep(1:2,each=360))

dim(newlewis)

library(dplyr)

filter(newlewis,item==67)