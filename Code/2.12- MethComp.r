#2.12
#Method Comparison

#load(MethComp)  ?

#Get Data
ox<-read.csv("c:/data/ox.csv")
#item - subject identifier
#repl - replicate value
#co - Oxygen Saturation  - co
#pulse - Oxygen Saturation  - Pulse

########################################################
#WinBUGS code for exercise 12 As always, each model has a node called "dumnode"
#which is the sum of the first value in each data field of the data array.
#This is necessary to overcome the fact that the first two models don't
#require data from all of the data fields in the model which, without the
#dummy node "dumnode", results in an error from WinBUGS.
#So summing the first value from each data field tricks WinBUGS into
#thinking all data fields are participating in the model.
#There are five variables in the data array at the bottom of this file, a
#description of these is provided in the assignment sheet.
#Several infants had only one (infant 39) or two (infants 17, 20, 25 and 50)
#measurements rather than the full three measurements, and these missing values
#have been replaced by "NA" codes in the array below which retains its full
#dimension of 5 variables x 61 infants subjects x 3 measurements per subject.
#The model can be checked from the local text and the data from the same
#array at the end of the text used to "load data"; just highlight "idno[]" with
#the mouse and click "load data", WinBUGS will find the array for itself!
#As usual, remember to click "gen inits" after "load inits" since WinBUGS will
#need to generate values for the missing data fields as well as producing
#starting values for other nodes. Given the models used WinBUGS will have no
#trouble generating initial values for the missing data fields and associated
#model nodes.
#Enquiries about the syntax will be happily answered by the course instructor,
#especially concerning model 3 if anyone is interested!

#Question 1
model
{
dumnode <- idno[1]+sample[1]+co.ox[1]+pulse.ox[1]+diff[1]
for (i in 1:183)
{
diff[i] ~ dnorm(0,tau.d)}
tau.d <- pow(sigma.d,-2)
sigma.d ~ dunif(0,1000)
sigma2.d <- pow(sigma.d,2) }

inits list(sigma.d=1)

#Question 2
model
{
dumnode <- idno[1]+sample[1]+co.ox[1]+pulse.ox[1]+diff[1]
for (i in 1:183) { diff[i] ~ dnorm(delta,tau.d) }
delta ~ dnorm(0,0.0001)
tau.d <- pow(sigma.d,-2)
sigma.d ~ dunif(0,1000)
agree.up <- delta+1.96*sigma.d
agree.low <- delta-1.96*sigma.d
sigma2.d <- pow(sigma.d,2) }

inits list(delta = 0, sigma.d=1)