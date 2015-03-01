#Exercise 2.7 - Assessing Covergence using the Gelman Rubin Statistic
#Schools Data

library(R2WinBUGS)
data(schools)
schools

J=length(schools)
#############################################
#part 1 - Bugs Model
cat(" model
     {
  for (j in 1:J){                          # J=8, the number of schools
    y[j] ~ dnorm (theta[j], tau.y[j])      # data model:  the likelihood
    tau.y[j] <- pow(sigma.y[j], -2)        # tau = 1/sigma^2
              }
  for (j in 1:J){
    theta[j] ~ dnorm (mu.theta, tau.theta) # hierarchical model for theta
            }
  tau.theta <- pow(sigma.theta, -2)        # tau = 1/sigma^2
  mu.theta ~ dnorm (0.0, 1.0E-6)           # noninformative prior on mu
  sigma.theta ~ dunif (0, 1000)            # noninformative prior on sigma
     }",
    file="m1.bug" )
########################################################################
#part 2 - declare data to be tested
schools.data <- list ("J", "y", "sigma.y")
schools.inits <- function()
  list (theta=rnorm(J,0,1), mu.theta=rnorm(1,0,100),
        sigma.theta=runif(1,0,100))
schools.parameters <- c("theta", "mu.theta", "sigma.theta")
########################################################################
#part 3  get BRugs program

numtimes <- 10
numiters <- 100

for(i in 1:numtimes)
{
m1.brugs <-
bugs( data = schools.data,
      inits = list (schools.inits(), schools.inits(), schools.inits()),
      param = schools.parameters,
      model = "m1.bug",
      n.chains = 3,
      n.iter = numiters,
      n.burnin = 0,
      n.thin = 1,
      program="openbugs",
      debug = FALSE,
      clearWD = TRUE )

ifelse( i == 1,
        matres <- as.matrix(m1.brugs$summary[,8]),
        matres <- cbind(matres,as.matrix(m1.brugs$summary[,8]) ) )
}
matres

#################################################################
#mcmc.list
#check fucntion is declared
m1.res=mcmc.list.bugs(m1.res)
summary(m1.res)
#################################################################


##############################################################

library( coda )
ex9mcmc1 <- mcmc(data = m1.res$sims.array[,1,])
ex9mcmc2 <- mcmc(data = m1.res$sims.array[,2,])
ex9mcmc3 <- mcmc(data = m1.res$sims.array[,3,])
ex9list <- mcmc.list(chain1 = ex9mcmc1, chain2 = ex9mcmc2, chain3 = ex9mcmc3)
gelman.diag(ex9list)
gelman.plot(ex9list)
geweke.diag(ex9list)
heidel.diag(ex9list)
raftery.diag(ex9list)
#######################################################################