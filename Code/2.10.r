# exercise 2-10-1

#U - yi1
#V - yi2
#W - yi1 + yi2
#X - yi1 - yi2

#0.5*var(U)*var(V)        sigsq.a + sigsq.e
#0.5*var(X)               sigsq.e

######################################

#get mgram file
mgram <-read.csv("c:/data/mgram.csv")

#0.5*[var(U)+var(V)]        sigsq.a + sigsq.e
sigsq.ae <- 0.5*(var(mgram$pdens1)+var(mgram$pdens2))

#######################################
#0.5*var(yi1 - yi2)               sigsq.e
sigsq.e <- 0.5*var(mgram$pdens1-mgram$pdens2)
sigsq.a =sigsq.ae - sigsq.e
sig.a=sqrt(sigsq.a)
sig.e=sqrt(sigsq.e)
mu.1=mean(mgram$pdens1)
mu.2=mean(mgram$pdens2)
mu.1
mu.2                                          #solns to part 2.10.1.a
sig.a
sig.e

#######################################
#use mu = 37
#get BUGS code of model - mgram.odc


cat( "model { for (i in 1:951)
          {
          pdens1[i] ~ dnorm(a[i],tau.e)
          pdens2[i] ~ dnorm(a[i],tau.e)
          a[i] ~ dnorm(mu,tau.a)
          }
    dumnode <- weight1[1] + weight2[1] + mz[1] + dz[1] + agemgram1[1] + agemgram2[1] + study[1]
    tau.a <- pow(sigma.a,-2)
    sigma.a ~ dunif(0,1000)
    tau.e <- pow(sigma.e,-2)
    sigma.e ~ dunif(0,1000)
    mu ~ dnorm(0,1.0E-6)
    sigma2.a <- pow(sigma.a,2)
    sigma2.e <- pow(sigma.e,2) }",file="m1.bug" )
#############################################################################
mgm.ini = list(mu = 37, sigma.a = 16, sigma.e = 13.5)


m1.res <-
bugs( data = mgram,
      inits = mgm.ini,
      param = c("mu","sigma.a","sigma.e","sigma2.a","sigma2.e"),
      model = "m1.bug",
      n.chains = 3,
      n.iter = 10000,
      n.burnin = 5000,
      n.thin = 5,
      program="openbugs",
      debug = FALSE,
      clearWD = TRUE )


m1.res

#############################################################################
# Question 2.10.2
#rho is now included in model
#mu becomes b.int

#model 2
cat( "model{
for (i in 1:951)
  {
  pdens1[i] ~ dnorm(mean.pdens1[i],tau.e)
  pdens2[i] ~ dnorm(mean.pdens2[i],tau.e)
  mean.pdens1[i] <- b.int + sqrt(rho)*a1[i] + sqrt(1-rho)*a2[i]
  mean.pdens2[i] <- b.int + sqrt(rho)*a1[i] + mz[i]*sqrt(1-rho)*a2[i] + dz[i]*sqrt(1-rho)*a3[i]
  a1[i] ~ dnorm(0,tau.a)
  a2[i] ~ dnorm(0,tau.a)
  a3[i] ~ dnorm(0,tau.a)
  }
dumnode <- weight1[1] + weight2[1] + mz[1] + dz[1] + agemgram1[1] + agemgram2[1] + study[1]
rho ~ dunif(0,1)
b.int ~ dnorm(0,0.0001)
tau.a <- pow(sigma.a,-2)
sigma.a ~ dunif(0,1000)
tau.e <- pow(sigma.e,-2)
sigma.e ~ dunif(0,1000)
sigma2.a <- pow(sigma.a,2)
sigma2.e <- pow(sigma.e,2)
}",file="m2.bug" )#end of model 2


#rho assigned value of 0.5
mgm2.ini = list(rho = 0.5, b.int = 37, sigma.a = 16, sigma.e = 13.5)



m2.res <-
bugs( data = mgram,
      inits = mgm2.ini,
      param = c("b.int","rho","sigma.a","sigma.e","sigma2.a","sigma2.e"),
      model = "m2.bug",
      n.chains = 3,
      n.iter = 20000,
      n.burnin = 10000,
      n.thin = 5,
      program="openbugs",
      debug = FALSE,
      clearWD = TRUE )


m2.res


#Q.2.10.2.A - Examine Output
#Q.2.10.2.B - Theory. (before:just mu. now:b.int and rho)
##############################################################################
#Question 2.10.3
#add parameter b.age
#parameters are(b.age,b.int, rho, and the sigmas)

##########################
# Starting Value
#
slr1=lm(mgram$pdens1~mgram$agemgram1)
slr2=lm(mgram$pdens2~mgram$agemgram2)
#starting value for b.age as -0.75

cat(" model
{ for (i in 1:951)
  {
  pdens1[i] ~ dnorm(mean.pdens1[i],tau.e)
  pdens2[i] ~ dnorm(mean.pdens2[i],tau.e)
  mean.pdens1[i] <- b.int + b.age*agemgram1[i] + sqrt(rho)*a1[i] + sqrt(1-rho)*a2[i]
  mean.pdens2[i] <- b.int + b.age*agemgram2[i] + sqrt(rho)*a1[i] + mz[i]*sqrt(1-rho)*a2[i] + dz[i]*sqrt(1-rho)*a3[i]
  a1[i] ~ dnorm(0,tau.a)
  a2[i] ~ dnorm(0,tau.a)
  a3[i] ~ dnorm(0,tau.a)
  }
  dumnode <- weight1[1] + weight2[1] + mz[1] + dz[1] + agemgram1[1] + agemgram2[1] + study[1]
rho ~ dunif(0,1)
b.int ~ dnorm(0,0.0001)
b.age ~ dnorm(0,0.0001)
tau.a <- pow(sigma.a,-2)
sigma.a ~ dunif(0,1000)
tau.e <- pow(sigma.e,-2)
sigma.e ~ dunif(0,1000)
sigma2.a <- pow(sigma.a,2)
sigma2.e <- pow(sigma.e,2)
}",file="m3.bug" )#end of model 3

mgrm3.ini=list(rho = 0.5, b.int = 37, b.age = -0.75, sigma.a = 16, sigma.e = 13.5)




m3.res <-
bugs( data = mgram,
      inits = mgm2.ini,
      param = c("b.int","rho","sigma.a","sigma.e","sigma2.a","sigma2.e"),
      model = "m2.bug",
      n.chains = 3,
      n.iter = 5000,
      n.burnin = 2000,
      n.thin = 5,
      program="openbugs",
      debug = FALSE,
      clearWD = TRUE )


m3.res


##############################################################################
#Model for Question 2.10.4
#Weight is included in Regression Model

# add parameter to the model b.wgt
# paramters are now b.int, b.age.b.wgt, rho and the sigmas



####################################
#Starting Values

slr1=lm(mgram$pdens1~mgram$weight1)
slr1
slr2=lm(mgram$pdens2~mgram$weight2)
slr2
#starting value of -0.64 is chosen
####################################


{
for (i in 1:951)
  {
  pdens1[i] ~ dnorm(mean.pdens1[i],tau.e)
  pdens2[i] ~ dnorm(mean.pdens2[i],tau.e)
  mean.pdens1[i] <- b.int + b.age*agemgram1[i] + b.wgt*weight1[i] + sqrt(rho)*a1[i] + sqrt(1-rho)*a2[i]
  mean.pdens2[i] <- b.int + b.age*agemgram2[i] + b.wgt*weight2[i] + sqrt(rho)*a1[i] + mz[i]*sqrt(1-rho)*a2[i] + dz[i]*sqrt(1-rho)*a3[i]
  a1[i] ~ dnorm(0,tau.a)
  a2[i] ~ dnorm(0,tau.a)
  a3[i] ~ dnorm(0,tau.a)
  }
dumnode <- weight1[1] + weight2[1] + mz[1] + dz[1] + agemgram1[1] + agemgram2[1] + study[1]
rho ~ dunif(0,1)
b.int ~ dnorm(0,0.0001)
b.age ~ dnorm(0,0.0001)
b.wgt ~ dnorm(0,0.0001)
tau.a <- pow(sigma.a,-2)
sigma.a ~ dunif(0,1000)
tau.e <- pow(sigma.e,-2)
sigma.e ~ dunif(0,1000)
sigma2.a <- pow(sigma.a,2)
sigma2.e <- pow(sigma.e,2)
}#end of model 4
inits list(rho = 0.5, b.int = 76, b.age = -0.75, b.wgt = -0.64, sigma.a = 16, sigma.e = 13.5)