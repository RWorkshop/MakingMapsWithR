
###############################################################

library(outliers)

sigma=1
sigma2=0.10
imb=1.0

n=500

EAD=numeric(n)
LOA=numeric(n)
out=numeric(n)

for (i in 1:n)
{
X=rnorm(20,50,sigma)
Y=rnorm(20,50+imb,sigma+sigma2)
Ave = (X+Y)/2
Diff = X-Y

#EAD
EAD[i]=mean(abs(Diff))
LOA[i]=1.96*sd(Diff)
out[i] = as.numeric(grubbs.test(Diff)$p.value<0.05)
}


u1=n-sum(out)
u2=u1+1
o=order(out)



set=cbind(EAD[o],LOA[o])
set1=set[1:u1,]
set2=set[u2:n,]
plot(set1, pch=17, col="red")
points(set2,pch=16, col="green")
###############################################################
