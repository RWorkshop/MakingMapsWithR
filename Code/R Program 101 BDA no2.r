

#Posterior Distribution
#gamma(3+x,5+y)

#1) get ASTHMA data
asthma <- read.table("c:/data/asthma.dat",header=TRUE)

###################################################################



#prior - gamma(3,5)
#posterior - gamma(3+y,5+x)
#y is the number of deaths in one year   - 3
#x is the exposure rate.     - 2
#approx 2 cases per 100,000
#Answer to part 1 - Gamma(6,7)

#Need to get ODC files

#Y
#Lambda
#Theta
#N



###################################################################
#BUGS CODE for part 2

#397 deaths per 20 million
#approx 20 per million

#approx 2 per 100,000
