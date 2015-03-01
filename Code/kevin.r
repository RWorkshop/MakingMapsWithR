# R code for entering the data and fitting the Bugs model for 8 schools
# analysis from Section 5.5 of "Bayesian Data Analysis".

# All graphs are output to a device called by the function "plt"
# Choose the appropriate definition of plt here by uncommenting:
#
# plt <- function( file, ... )          pdf( paste(file,"pdf",sep="."), ... )
# plt <- function( file, ... ) win.metafile( paste(file,"emf",sep="."), ... )
# plt <- function( file, ... )   postscript( paste(file,"eps",sep="."), ... )
# plt <- function( file, ... )          X11() # Output to the screen

# Load the library allowing you to access WinBUGS from within R
#

library( R2WinBUGS )

# Define where WinBUGS in installed on your computer:
# --- this is used when invoking bugs() from the R2WinBUGS package.
# bd <- "C:/Program Files/WinBUGS14/"

bd <- "c:/Stat/Bugs/WinBUGS14/"

# First get the data and generate component objects

#schools <- read.table("schools.dat", header = T)
#J <- nrow(schools)
#y <- schools$estimate
#sigma.y <- schools$sd

# OR generate the data from scratch

J <- 8
school <- c("A","B","C","D","E","F","G","H")
y <- c(28,8,-3,7,-1,1,18,12)
sigma.y <- c(15,10,16,11,9,11,10,18)

#*********************** Model 1 ******************************************

# Bugs code for model 1 with prior distribution on sigma.theta
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

schools.data <- list ("J", "y", "sigma.y")
schools.inits <- function()
  list (theta=rnorm(J,0,1), mu.theta=rnorm(1,0,100),
        sigma.theta=runif(1,0,100))
schools.parameters <- c("theta", "mu.theta", "sigma.theta")

m1.brugs <-
bugs( data = schools.data,
      inits = list (schools.inits(), schools.inits(), schools.inits()),
      param = schools.parameters,
      model = "m1.bug",
      n.chains = 3,
      n.iter = 30000,
      n.burnin = 20000,
      n.thin = 5,
      program="openbugs",
      debug = FALSE,
      clearWD = TRUE )$sim
shell( "del m1.bug" )

s

# Take a look at the output from winbugs
str( m1.res )

#************** PLOT 1 ***********************

plt( "sol7-1" )

hist( m1.res[,1,"sigma.theta"], breaks = seq(0,70,2),
      freq = FALSE, xlim = c(0,35), col=gray(0.7),
      xlab = "sigma.theta", ylab = "density",
      main = "Prior and posterior distribution of sigma.theta")
segments(0,1/35,50,1/35)

#*********************************************

# statistics for prior and posterior mean

obsmean <- sum(y/((sigma.y)^2))/sum(1/((sigma.y)^2))
obssigma <- sqrt(1/sum(1/((sigma.y)^2)))
allmean.theta <- mean(m1.res[,1:3,9])
all025.theta <- quantile(m1.res[,1:3,9],probs = 0.025)
all975.theta <- quantile(m1.res[,1:3,9],probs = 0.975)

# set up vectors for posterior mean, 2.5% and 97.5% quantiles for school specific theta

postmean.theta <- numeric(8)
post025.theta <- numeric(8)
post975.theta <- numeric(8)

for(i in 1:8)
{
postmean.theta[i] <- mean(m1.res[,1:3,i])
post025.theta[i] <- quantile(m1.res[,1:3,i],probs = 0.025)
post975.theta[i] <- quantile(m1.res[,1:3,i],probs = 0.975)
}

#************** PLOT 2 ***********************

plt( "sol7-2" )

# plot the posterior mean for each school against the raw score
# to demonstrate the magnitude of the shrinkage and the
# uncertainty in the posterior means

plot( y, postmean.theta,
      ylim = c(-10,35), xlim = c(-10,35), pch = 16, cex = 1.5,
      ylab = expression("Posterior mean of "*theta[i]*" with 95% credible interval"),
      xlab = "Observed score")
# title("Observed versus posterior expected score")
abline(h = mean(m1.res[,1:3,9]), col = "red", lwd = 2)
abline(0,1,lty = 2, lwd = 2)
segments(y,post025.theta,y,post975.theta, lwd = 2)

# Caption: Posterior means and 95% credible intervals for school specific
# parameters theta[i] plotted against the obsevred mean score for each school.
# The shallow gradient of the relationship indicates the extent of the shrinkage
# of the posterior means towards the combined effect.

#-------------------------------------------------------------------------------
# PLOT 3
# Plot the "meta-analysis" diagram with estimated effect size and 95% intervals
# for each school

plt( "sol7-3" )

dotchart( c(y,obsmean,postmean.theta,allmean.theta),
          groups = rep(1:9,2), labels = c(school,"ALL"), lcolor = "white",
          pch = 16, cex = 1.2, xlim = c(-60,60),
          xlab = expression("Score (y) and posterior mean "*(theta)*" with 95% intervals"),
          ylab = "School")
arrows( y-1.96*sigma.y, 37-(1:8*4), y+1.96*sigma.y, 37-(1:8*4),
        lty = 1, lwd = 3, length = 0.05, angle = 90, code = 3 )
arrows( post025.theta, 38-(1:8*4), post975.theta, 38-(1:8*4),
        lty = 2, lwd = 3, length = 0.05, angle = 90, code = 3 )
arrows(obsmean-1.96*obssigma,1,obsmean+1.96*obssigma,1,
       lty = 1, lwd = 3, length = 0.05, angle = 90, code = 3)
arrows(all025.theta,2,all975.theta,2,
       lty = 2, lwd = 3, length = 0.05, angle = 90, code = 3)
#title("Prior and posterior estimated scores for the schools example")
abline(v = mean(m1.res[,,9]), lty = 3, lwd = 2, col = "red")
#abline(v = obsmean, lty = 3, lwd = 2, col = "blue")
legend("topleft",legend = c("Random","Fixed","Posterior mean"),bty="n",
       lty = c(2,1,3), lwd = c(2,2,2), col = c("black","black","red"))

# Caption: Observed mean scores and 95% confidence intervals for each school
# (fixed effects model) alongside posterior means and 95% credible intervals
# from the random effects model.  Note that the posterior means are shrunk
# towards the combined mean, and that the 95% posterior credible intervals
# are narrower than the fixed effect confidence intervals due to the additional
# precision that results from assuming the school means are drawn from a common
# population distribution.

#-------------------------------------------------------------------------------
# Model 2
# Note the need to re-define schools.init

# Bugs code for model 2 with prior distribution on tau.theta
cat(" model
    {
  for (j in 1:J){                          # J=8, the number of schools
    y[j] ~ dnorm (theta[j], tau.y[j])      # data model:  the likelihood
    tau.y[j] <- pow(sigma.y[j], -2)        # tau = 1/sigma^2
              }
  for (j in 1:J){
    theta[j] ~ dnorm (mu.theta, tau.theta) # hierarchical model for theta
            }
  tau.theta ~ dgamma(1,1)             # Gamma prior on tau
  mu.theta ~ dnorm (0.0, 1.0E-6)      # noninformative prior on mu
  sigma.theta <- 1/sqrt(tau.theta)    # sigma.theta = inverse of square root of precision tau
    }",
    file="m2.bug" )

schools.inits <- function()
  list (theta=rnorm(J,0,1), mu.theta=rnorm(1,0,100),
        tau.theta=rgamma(1,shape = 1, rate = 1))

m2.res <-
bugs( data = schools.data,
      inits = list (schools.inits(), schools.inits(), schools.inits()),
      param = schools.parameters,
      model = "m2.bug",
      n.chains = 3,
      n.iter = 30000,
      n.burnin = 20000,
      n.thin = 5,
      bugs.directory = bd,
      debug = FALSE,
      clearWD = TRUE )$sims.array
shell( "del m2.bug" )

plt( "sol7-4" )
hist(m2.res[,1,10], breaks = seq(0,15,0.25), freq = FALSE, xlim = c(0,10),
     xlab = expression(sigma[theta]), ylab = "Density", col=gray(0.7),
     main = expression("Prior and posterior distribution of "*sigma[theta]) )
lines(seq(0.25,12,0.25),2*exp(-1/(seq(0.25,12,0.25)^2))/(seq(0.25,12,0.25)^3),lwd=3)
