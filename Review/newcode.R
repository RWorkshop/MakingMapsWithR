

collect =c()

for (i in 1:100000)
{
set.seed(i)
X=rnorm(15,mean=100,sd=4)
Y=rnorm(15,mean=100.5,sd=3.2)
Z1 = cor(X,Y)
W = which.max(cooks.distance(lm(Y~X)))
Z2 = cor(X[-W],Y[-W])
diff = Z1-Z2
if (Z1 > 0.78) {
	collect=c(collect,i,Z1,Z2,diff)
 }
}

cs=4
rs=length(collect)/cs

collect = matrix(collect,nrow=rs,ncol=cs,byrow=T)



############################################


X=rnorm(9,mean=100,sd=4)
Y=rnorm(9,mean=100.5,sd=3.2)

while( cor(X,Y) < 0.5) { 
	
X=rnorm(9,mean=100,sd=4)
Y=rnorm(9,mean=100.5,sd=3.2)
}


cor(X,Y)
plot(X,Y)
X1 = c(X,118)
Y2 = c(Y,117)
cor(X1,Y2)
X1 = c(X,128)
Y1 = c(Y,127)
cor(X1,Y1)
