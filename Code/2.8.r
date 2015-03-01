model
{
for (j in 1:8)
  {
  y[j] ~ dnorm(theta[j], prec.y[j])
  theta[j] ~ dnorm(mu.theta, prec.theta)
  prec.y[j] <- pow(sigma.y[j], -2) }
  mu.theta ~ dnorm(0,1.0E-6)
  prec.theta <- pow(tau.theta, -2)
  tau.theta <- 0.41
  #tau.theta ~ dunif(0,1000)
  }
# INITS
}

list(mu.theta = 0) #list(mu.theta = 0, tau.theta=0.41)
####################################################################



