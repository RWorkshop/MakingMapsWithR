
#Linear Mixed Effets Models of Fetal Growth

library( R2WinBUGS )
library( BRugs )
library( nlme )

########################################################
hc<-read.csv("c:/data/fetal.csv")
fetal


#########################################################

model
{
for (i in 1:707)
{
u[i,1:2] ~ dmnorm(zero.beta[],Omega.beta[,]);
sub.beta[i,1] <- mu.beta[1] + u[i,1];
sub.beta[i,2] <- mu.beta[2] + u[i,2];
}

recov <- sub.beta[1,1]*sub.beta[1,2];
dumnode <- ga[1] + hc[1];

for (j in 1:3097)
{
mu[j] <- (mu.beta[1] + u[id[j],1]) + (mu.beta[2] + u[id[j],2])*(X[j]);
Y[j] ~ dnorm(mu[j],tau.e);
}

zero.beta[1] <- 0
zero.beta[2] <- 0
mu.beta[1] ~ dnorm(0.0,1.0E-5);
mu.beta[2] ~ dnorm(0.0,1.0E-5);
tau.e <- 1/pow(sigma.e,2);
sigma.e ~ dunif(0,10);
sigma2.e <- pow(sigma.e,2);
Omega.beta[1:2,1:2] ~ dwish(R[,],2);
R[1,1] <- 0.5;    R[1,2] <- 0;
R[2,1] <- 0;    R[2,2] <- 0.1;
Sigma2.beta[1:2,1:2] <- inverse(Omega.beta[,]);
rancov <- Sigma2.beta[1,2] /
(sqrt(Sigma2.beta[1,1])*(sqrt(Sigma2.beta[2,2])));
} #end model

#inits

list( mu.beta = c(0,0), sigma.e = 0.2,
Omega.beta = structure(.Data = c(1,0,0,1), .Dim = c(2, 2)))

#  Estimates and standard errors for the head circumference model
#  lambda in the Box-Cox Transformation is 0.5, so Y = SQRT(HC)
#  transformation of gestational age is
#  X = g(GA) = GA - 0.0116638*GA^2
#  Estimates from MLwiN, RIGLS with random intercept, linear and covariance
#  Standard errors in brackets
#  FIXED PARAMETERS:
#   intercept = -0.08372262 (0.04438324)
#   linear    =  0.86843520 (0.00238596)
#  RANDOM PARAMETERS (variances):
#   intercept  =  0.65512600 (0.07485665)
#   linear     =  0.00185621 (0.00021622)
#   covariance = -0.03317791 (0.00396940)
#   residual   =  0.04910566 (0.00165977)

########################################################
#Step 4
cat("model
	 {
  	for (i in 1:707)
  	{
  	u[i,1:2] ~ dmnorm(zero.beta[],Omega.beta[,]);
  	sub.beta[i,1] <- mu.beta[1] + u[i,1];
  	sub.beta[i,2] <- mu.beta[2] + u[i,2];
  	}
	recov <- sub.beta[1,1]*sub.beta[1,2];
  	for (j in 1:3097)
  	{
  	mu[j] <- (mu.beta[1] + u[id[j],1]) + (mu.beta[2] + u[id[j],2])*(X[j]);
  	Y[j] ~ dnorm(mu[j],tau.e);
  	}
	zero.beta[1] <- 0
	zero.beta[2] <- 0
	mu.beta[1] ~ dnorm(0.0,1.0E-5);
	mu.beta[2] ~ dnorm(0.0,1.0E-5);
	tau.e <- 1/pow(sigma.e,2);
	sigma.e ~ dunif(0,10);
	sigma2.e <- pow(sigma.e,2);
	Omega.beta[1:2,1:2] ~ dwish(R[,],2);
	R[1,1] <- 0.5;    R[1,2] <- 0;
	R[2,1] <- 0;    R[2,2] <- 0.1;
	Sigma2.beta[1:2,1:2] <- inverse(Omega.beta[,]);
	rancov <- Sigma2.beta[1,2] /
	(sqrt(Sigma2.beta[1,1])*(sqrt(Sigma2.beta[2,2])));
	}",
      file="m1.bug" )
########################################################
fetal.ini <- list( mu.beta = c(0,0), sigma.e = 0.2,
Omega.beta = structure(.Data = c(1,0,0,1), .Dim = c(2,2)))


########################################################
#Step 3
m1.brugs <-
bugs( data = list( id = hc$ID, X = hc$TGA, Y = hc$SQRTHC ),
      inits = list(fetal.ini),
      param = c("mu.beta","Sigma2.beta","sigma.e"),
      model = "m1.bug",
      n.chains = 1,
      n.iter = 2000,
      n.burnin = 1000,
      n.thin = 1,
      program="openbugs",
      debug = FALSE,
      clearWD = TRUE )
########################################################
#Step 4 Convert to list formar

########################################################
#Step 5 Summary Statisitcs



# Question 1: Generate a summary of the output
m1.res$summary

# Question 2:



