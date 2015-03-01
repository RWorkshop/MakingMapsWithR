#Metropolis Hastings

rho<-0.7
nsim<-1000
ans<-matrix(NA,nr=nsim,nc=2)#matrix of two cols, 1000 rows (or numsim rows)
Sigma<-matrix(c(1,rho,rho,1),nr=2)                #correlation matrix
Sigma<-solve(Sigma)                               #inverse of Sigma

x1<-x2<-30                  #initialise x values at 30
xcurr<-c(x1,x2)             #initialise a vector to hold current x values

accept<-numeric(nsim)       #initialise an acceptance vector
sigma<-2                    #set Std.Dev to 2

for(ii in 1:nsim)
{
  xprop<-xcurr+rnorm(2,mean=0,sd=sigma)           #initialise proposal
  logkxprop<- -t(xprop)%*%SigmaInv%*%xprop/2      #loglikelihood of proposal
  logkxcurr<- -t(xcurr)%*%SigmaInv%*%xcurr/2      #loglikelihood of current vector
  
  alpha<-min(1,exp(logkxprop-logkxcurr))
  u<-rnorm(1)
   if (accept[ii] <- (u<alpha)){xaccept<-xprop}
   else{xaccept<-xcurr}
   
   ans[ii,]<-xaccept
   xcurr<-xaccept
}
############################################################

cat("Accepted Proposals:",sum(accept)/nsims,"\n")

############################################################
#plotting examples
pairs(ans)
matplot (ans,type="l")
pairs(ans[-(1:100),])
par(mfrom=c(2,2))
pacf(ans[,1])              #partical autocorrelations
pacf(ans[,2])
acf(ans[,1])               #autocorrelations functions
acf(ans[,2])

###########################################################
