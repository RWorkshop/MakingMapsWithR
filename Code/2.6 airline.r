
airline <- read.csv( "c:/data/airline.csv" )
airline
###############################################################################
#prior - no0n-informative Gamma(alpha,beta)
#noninformative - 0,0
#posterior - Gamma(alpha + sum_y,beta+n)
#n is number of samples - 26
#sum y is number of deaths

names(airline)
n=length(airline$fatal)  #n
sum_y=sum(airline$fatal)  #sum_y

alpha=0
beta=0


post.mean = (alpha + sum_y)/(beta + n)
post.mean
###############################################################################

#dnbinom(x, size, prob, mu, log = FALSE)
astar = (alpha + sum_y)
bstar = (beta + n)

#prior - gamma(astar,bstar)

rg= rgamma( 100, astar, bstar )

curve( dgamma( x, astar, bstar ), from=20, to=30 )

# supposed mean for 2002 is as follow
( fbar <- mean( rg <- rgamma( 10000, astar, bstar ) ) )
fbar
abline(v=fbar, col="red")
##################################################
N=1000
theta <- rgamma( N, 634, 26 )
y.2002<- rpois( N, theta )
hist(y.2002)
##################################################
cat("model
{
for (i in 1:I)
	{
	fatal[i]~dpois(mu)
	}
	mu~dgamma(0,0)
}file="a1.bug")

a1.ini<-list(list(mu=22),list(mu=23),list(mu=24))
a1.dat<-list(fatal =c(airline$fatal,NA), I=27
a1.res<-bugs(data=a1.dat,
		inits=a1.ini,
		param=c("mu","fatal[27]"),
		model="a1.bug",
		n.chains=3,
		n.iter=30000,
		n.burnin=20000,
		n.thin=5,
		program="openbugs",
		debug=FALSE,
		clearWD=TRUE)
