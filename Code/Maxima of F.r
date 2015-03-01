time1=Sys.time()


sumv=c(0,0,0,0)
test=c(0,0,0,0)
P=1000


for(k in 1:P)
  {
  M=10000
  x<-rf(M,1,9)
  y<-rf(M,1,9)
  w<-numeric(M)
  for(i in 1:M){w[i]=min(x[i],y[i])}
  sumv[1]=sumv[1]+quantile(w,0.25)
  sumv[2]=sumv[2]+quantile(w,0.5)
  sumv[3]=sumv[3]+quantile(w,0.75)
  sumv[4]=sumv[4]+quantile(w,0.95)
  }
sumv=sumv/P
sumv
test[1]=qf(1-sqrt(1-(0.25)),1,9)
test[2]=qf(1-sqrt(1-(0.5)),1,9)
test[3]=qf(1-sqrt(1-(0.75)),1,9)
test[4]=qf(1-sqrt(1-(0.95)),1,9)
test
#####################################################################

M=1000
x<-numeric(M)
y<-numeric(M)
z<-numeric(M)
mean.95<-numeric(M)
######################


x<-rf(M,1,9)
y<-rf(M,1,9)
for(i in 1:M){z[i]=max(x[i],y[i])}
mean.95[1]=quantile(z,0.95)
new.95=quantile(z,0.95)

#loop
for (k in 2:1000)
  {
  l=k-1
  x<-rf(M,1,9)
  y<-rf(M,1,9)
  for(i in 1:M){z[i]=max(x[i],y[i])}
  new.95=quantile(z,0.95)
  mean.95[k]=((mean.95[l]*l)+new.95)/k
  }



time2=Sys.time()

####################################################################

xs=sort(x)
ys=sort(y)
k=round(M*(sqrt(0.5)))
l=round(M*(sqrt(0.95)))
xs[k]
ys[k]
xs[l]
ys[l]

j=sqrt(0.95)
qf(j,1,9)