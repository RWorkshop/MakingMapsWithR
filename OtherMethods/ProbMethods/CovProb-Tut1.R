# www4.stat.ncsu.edu/~reich/st740/CredSets.doc‎
# http://statistics.unl.edu/faculty/yang/agreement.pdf
library("binGroup")

n.p<-100   #Number of true proportions to consider
M<-10000   #Number of simulated data sets
n<-5       #Sample size

#############################################################
#Set up the matrices to store the results
p.grid<-seq(0,1,length=n.p)
cov.prob<-width<-matrix(0,n.p,4)
dimnames(cov.prob)[[2]]<-c("Exact","Wald","Score","Bayes")
dimnames(cov.prob)[[1]]<-paste("p =",round(p.grid,2))
dimnames(width)<-dimnames(cov.prob)

#############################################################
#Function to compute the HPD Bayes credible set
binHPD<-function(n,y,a=1,b=1,conf.level=0.95){
   probs<-seq(0,1-conf.level,.001)
   l<-0;u<-1
   for(s in 1:length(probs)){
      l.can<-qbeta(probs[s],y+a,n-y+b)
      u.can<-qbeta(probs[s]+conf.level,y+a,n-y+b)
      if(u.can-l.can<u-l){
        u<-u.can
        l<-l.can
      }
   }
c(l,u)}

#############################################################

for(j in 1:n.p){for(s in 1:M){
   y<-rbinom(1,n,p.grid[j])
 
   #Exact
   CI<-binCP(n,y)
   cov.prob[j,1]<-cov.prob[j,1]+(CI[1]<=p.grid[j] & CI[2]>=p.grid[j])
   width[j,1]<-width[j,1]+CI[2]-CI[1]

   #Wald
   CI<-binWald(n,y)
   cov.prob[j,2]<-cov.prob[j,2]+(CI[1]<=p.grid[j] & CI[2]>=p.grid[j])
   width[j,2]<-width[j,2]+CI[2]-CI[1]

   #Score
   CI<-binWilson(n,y)
   cov.prob[j,3]<-cov.prob[j,3]+(CI[1]<=p.grid[j] & CI[2]>=p.grid[j])
   width[j,3]<-width[j,3]+CI[2]-CI[1]

   #Bayes
   CI<-binHPD(n,y)
   cov.prob[j,4]<-cov.prob[j,3]+(CI[1]<=p.grid[j] & CI[2]>=p.grid[j])
   width[j,4]<-width[j,4]+CI[2]-CI[1]

}}
#############################################################
cov.prob<-cov.prob/M
width<-width/M

#############################################################

plot(0,0,xlim=range(p.grid),ylim=range(cov.prob),col=0,ylab="Coverage prob",xlab="True proportion")
for(j in 1:4){lines(p.grid,cov.prob[,j],lty=j,col=j)}
lines(c(0,1),c(0.95,0.95))
legend("bottom",c("Exact","Wald","Score","Bayes"),lty=1:4,col=1:4,inset=0.05)

plot(0,0,xlim=range(p.grid),ylim=range(width),col=0,ylab="Average interval width",xlab="True proportion")
for(j in 1:4){lines(p.grid,width[,j],lty=j,col=j)}
legend("bottom",c("Exact","Wald","Score","Bayes"),lty=1:4,col=1:4,inset=0.05)

