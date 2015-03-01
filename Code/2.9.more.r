cat( "model
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
	rancov <- Sigma2.beta[1,2] / (sqrt(Sigma2.beta[1,1])*(sqrt(Sigma2.beta[2,2])));
	}",file="m1.bug" )
#inits

fetal.ini=list( mu.beta = c(0,0), sigma.e = 0.2,Omega.beta = structure(.Data = c(1,0,0,1), .Dim = c(2, 2)))

############################################################

m1.res <-
bugs( data = list(id=hc$ID,X=hc$TGA,Y=hc$SQRTHC),
      inits = fetal.ini,
      param = c("mu.beta","Sigma2.beta","sigma.e"),
      model = "m1.bug",
      n.chains = 3,
      n.iter = 300,
      n.burnin = 200,
      n.thin = 5,
      program="openbugs",
      debug = FALSE,
      clearWD = TRUE )

m1.res$summary

m1.res <- mcmc.list.bugs(m1.res)
str(m1.res)
m1.res$summary


###########################################################
linmod<-lme(SQRTHC~1+TGA,data=fetal,random=~1+TGA|ID)
summary(linmod)


m2.res <-
bugs( data = list(id=hc$ID,X=hc$TGA,Y=hc$SQRTHC),
      inits = fetal.ini,
      param = c("mu.beta","Sigma2.beta","sigma.e","rancov"),
      model = "m1.bug",
      n.chains = 3,
      n.iter = 300,
      n.burnin = 200,
      n.thin = 5,
      program="openbugs",
      debug = FALSE,
      clearWD = TRUE )
m2.res$summary









