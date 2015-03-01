zy#################################################
#Example 2.1          Simple Linear Regession in R
#
#



#################################################
#Example 2.1          Simple Linear Regession in R
x=c(1,2,3,4,5,6)
y=c(1,3,3,3,5,7)
slr1=lm(y~x)
slr1

###########################################################
#part 1 - set up the following
#a) data - a list
#b) initial values - a list of lists
#c) parameters to monitor

reg.dat = list(x=x,y=y,I=6)
#i.e. the List of Lists with candidate values
#for regressions this is 'alpha','beta' and 'sd'

reg.ini =  list (
list(alpha = 0.05, beta=0.9,sigma=0.8 ),
list(alpha = 0.055, beta=1,sigma=0.85 ),
list(alpha = 0.065, beta=1.1,sigma=1.1 ))

reg.par=c("alpha","beta","sigma")
###########################################################
#part 2 - set up Bugs model by 'cat' method

cat("model
  {
  for (i in 1:I)
      {
      y[i]~dnorm(mu[i],tau)
      mu[i]<-alpha+beta*x[i]
      }
    alpha ~ dnorm(0,1.0E-6)
    beta ~ dnorm(0,1.0E-6)
    sigma ~ dunif(0,100)
    tau<-1/pow(sigma,2)
  }", file="reg.bug")
  
class(reg.bug)
###########################################################
#part 3 - use the 'Bugs()' function to initialise reg.res

reg.brugs <-
bugs( data=reg.dat,
  inits=reg.ini,
  param=reg.par,
  model="reg.bug",
  n.chains=3,
  n.iter=20000,
  n.burnin=10000,
  n.thin=5,
  program="openbugs",
  debug=FALSE,
  clearWD=TRUE)
  
class(reg.brugs)
  
###########################################################
#part 4 - use the PDAwBuR to convert to correct Data Format (mcmc lists)
reg.brugs <- mcmc.list.bugs(reg.brugs)



###########################################################
#Part 5 - Examin Summary . (use the summary() function)
summary(reg.brugs)

#################################################
#births
library(Epi)
data(births)
births <- subset(births,!is.na(gestwks))

slr=lm(births$bweight ~ I(births$gestwks-35))

nrow(births)
reg.dat = list(x=births$gestwks-35,y=births$bweight,I=nrow(births))

#i.e. the List of Lists with candidate values
#for regressions this is 'alpha','beta' and 'sd'

reg.ini =  list (
list(alpha = 400,  beta=190, sigma=400),
list(alpha = 2500,  beta=200,   sigma=450 ),
list(alpha = 2600,  beta=205, sigma=475))

reg.par=c("alpha","beta","sigma")


cat("model
  {
  for (i in 1:I)
      {
      y[i]~dnorm(mu[i],tau)
      mu[i]<-alpha+beta*x[i]
      }
    alpha ~ dnorm(0,1.0E-6)
    beta ~ dnorm(0,1.0E-6)
    sigma ~ dunif(0,10000)
    tau<-1/pow(sigma,2)
  }", file="bth.bug")
  
  

bth.res <-bugs( data=reg.dat,
  inits=reg.ini,
  param=reg.par,
  model="bth.bug",
  n.chains=3,
  n.iter=20000,
  n.burnin=10000,
  n.thin=5,
  program="openbugs",
  debug=FALSE,
  clearWD=TRUE)

 #part 4 - use the PDAwBuR to convert to correct Data Format (mcmc lists)
bth.res <- mcmc.list.bugs(bth.res)


xyplot(reg.res)
xyplot(bth.res)
acfplot(reg.res)



###########################################################
#Part 5 - Examin Summary . (use the summary() function)
summary(bth.res)
  
