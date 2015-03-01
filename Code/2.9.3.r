########################################################
#Exercise 2.9 part 3

#Conditional and Unconditional distributions
#HC at 38 Weeks
cat( "model
	{
                  	for (i in 1:709)
	{
	u[i,1:2] ~ dmnorm(zero.beta[],Omega.beta[,]);
	sub.beta[i,1] <- mu.beta[1] + u[i,1];
	sub.beta[i,2] <- mu.beta[2] + u[i,2];
	}

	recov <- sub.beta[1,1]*sub.beta[1,2];

	for (j in 1:3100)
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
	rancov <- Sigma2.beta[1,2] / (sqrt(Sigma2.beta[1,1])*(sqrt(Sigma2.beta[2,2])));
	}",file="m3.bug" )
	

m3.res <-
bugs( data = list(id=c(hc$ID,708,708,709),X=c(hc$TGA,14.47,21.20,21.20),Y=c(hc$SQRTHC,11.18,NA,NA)),
      inits = fetal.ini,
      param = c("mu.beta","Sigma2.beta","sigma.e","Y[3099]","Y[3100]"),
      model = "m3.bug",
      n.chains = 3,
      n.iter = 900,
      n.burnin = 400,
      n.thin = 5,
      program="openbugs",
      debug = FALSE,
      clearWD = TRUE )


m3.res=mcmc.list.bugs(m3.res)
summary(m3.res)
