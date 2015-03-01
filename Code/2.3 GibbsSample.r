
#condtional probabilities



y1=0
y2=0
rho=0.8

#############################################
#E(Theta1/Theta2,y)
ETh1=y1+(rho*(Th2-y2))
Vth=1-(rho^2)
ETh2=y2+(rho*(Th1-y1))
Vth=1-(rho^2)
############################################





#Example 2.3
#Gibbs Sampler in R page 17


numsims<-1000
rho=0.8
tau1=sqrt(1-(rho^2))
theta1<-numeric(numsims)
theta2<-numeric(numsims)

#initialise theta1 and theta2
theta1[1]=-3
theta2[1]<-rnorm(1,mean=rho*theta1[1],sd=tau1)
theta1[1]
theta2[1]

for(i in 2:numsims)
{
theta1[i]<-rnorm(1,mean=rho*theta2[i-1],sd=tau1)
theta2[i]<-rnorm(1,mean=rho*theta1[i],sd=tau1)
}

mean(theta1[501:1000])
mean(theta2[501:1000])
sqrt(var(theta1[501:1000]))
sqrt(var(theta2[501:1000]))



################################################








theta[2]<-rnorm(1,mean=rho*theta1[1],sd=sqrt(1-(rho^2)))
for (i in 2:numsims)
{
theta1[i]=rnorm(1,rho*theta2[i-1],sd=sqrt(1-(rho^2)))
theta1[i]=rnorm(1,rho*theta1[i-1],sd=sqrt(1-(rho^2)))
}
###########################################################


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


