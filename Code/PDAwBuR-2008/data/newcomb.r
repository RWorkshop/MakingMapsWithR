#speed of light

newcomb <-
c(28, 26, 33, 24, 34, -44, 27, 16, 40, -2, 29, 22, 24, 21, 25, 
30, 23, 29, 31, 19, 24, 20, 36, 32, 36, 28, 25, 21, 28, 29, 37, 
25, 28, 26, 30, 32, 36, 26, 30, 22, 36, 23, 27, 27, 28, 27, 31, 
27, 26, 33, 26, 32, 32, 24, 39, 28, 24, 25, 32, 25, 29, 27, 28, 
29, 16, 23)
##################################################################
n=length(newcomb)#66
ybar=mean(newcomb)#26.2
s <- sqrt(var(newcomb))


tstar = qt(0.975,65)
CI=c(0,0)
CI[1]=ybar-((tstar*s)/(sqrt(n)))
CI[2]=ybar+((tstar*s)/(sqrt(n)))
CI

#calculate a central posterior interveal using distributionla results


#initiate Number of Iterations
N=1000

#draw sigmasq out of InvChiSq(n-1,s^2)  then get mu
sigmasq <- (65*(s^2))/( rchisq(N,n-1))
mu <- rnorm( N, mean = ybar, sd = sqrt(sigmasq/n))

quantile(mu,0.95 )
quantile(sigmasq,0.95)





































# Simon Newcomb's measurements of the speed of light, from Stigler
# (1977).  The data are recorded as deviations from $24800$
# nanoseconds.  Table 3.1 of Bayesian Data Analysis.

 ybar <- mean(data) )
( s <- sqrt(var(data)) )
n <- 66
Ns <- 1000 # number of simulations


















sigma2 <- ((n-1)*(s^2))/( rchisq(Ns,n-1,ncp=0))
mu     <- rnorm( Ns, mean = ybar, sd = sqrt(sigma2/n))
quantile(     mu, probs=c(5,50,95)/100 )
quantile( sigma2, probs=c(5,50,95)/100 )

plot( "newcomb" )
par( mar=c(3,3,1,1), mgp=c(3,1,0)/1.6 )
hist( newcomb, breaks = seq(-50,40,2),
      xlab = "Speed of light", main = "", col=gray(0.7))
