
#######################################################################
#Chapter 2 - Single Parameter Models
#Alberts page 40 - Football Scores
library(LearnBayes)
data(footballscores)
attach(footballscores)
d=favourite-underdog-spread
n=length(d)
v=sum(d^2)

#simulate 1000 values
P=rchisq(1000,n)/v
s=sqrt(1/P)
hist(s,main="")

