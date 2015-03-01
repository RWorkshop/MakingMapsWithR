#############################################################
#set working directory as C:/WD/
setwd('c:/WD') 
##############################################################
#BA99 Systolic Blood Pressure Data
#JSR DATA
  #765(9 times 85) observations
  #85 Subjects
  #3 Methods (JSR)
  #3 replications

#Run the following code only once
BA99.data<- read.csv(file="BA99.csv",head=FALSE,sep=",")
#BA99.data<-BA99.data[,-1]
#colnames(BA99.data)<-c("j1","j2","j3","s1","s2","s3","r1","r2","r3")
#BA99.data

obs<-c(BA99.data[,1],BA99.data[,2],BA99.data[,3],BA99.data[,4],
 BA99.data[,5],BA99.data[,6],BA99.data[,7],BA99.data[,8],BA99.data[,9])

method<-c(rep("J",(3*85)),rep("S",(3*85)),rep("R",(3*85)))
method=factor(method)
repl<-c(rep(1,85),rep(2,85),rep(3,85),rep(1,85),rep(2,85),rep(3,85),
rep(1,85),rep(2,85),rep(3,85))
seq2<-c(1:85)
subj=c(rep(seq2,9))
lm(obs~method)    #indicates bias

                                          

lmer(obs ~ 1 + method + (-1+repl|subj))

##############################################################
#BA99 Systolic Blood Pressure Data
#Wright V Mini Data
#68 (4 times 17) observations
  #17 Subjects
  #2 Methods (W and M)
  #2 replications

#Run the following code only once
BA86.data<- read.csv(file="BA86flows.csv",head=TRUE,sep=",")
BA86.data
BA86.obs<-c(BA86.data[,1],BA86.data[,2],BA86.data[,3],BA86.data[,4])

method<-c(rep("w",34),rep("m",34))
method=factor(method)
repl<-c(rep(1,17),rep(2,17),rep(1,17),rep(2,17))
seq2<-c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q")
subj=c(rep(seq2,4))
lm(BA86.obs~method)    #indicates bias

BA86<-data.frame(BA86.obs,method,subj)          
interaction.plot(method,subj,BA86.obs, las =1)

library(nlme)
fit1<-lme(BA86.obs ~ method, data =BA86, random = ~1|subj)
summary(fit1)
fit2<-lme(BA86.obs ~ method, data =BA86, random = ~1|subj/method)
fit3<-lme(BA86.obs ~ 1+ method, data =BA86, random = ~1|subj)
fit4<-lme(BA86.obs ~ 1+ method, data =BA86, random = ~ -1 + 1|subj )

#############################################################
# Design Matrices for BA86
Xmat=cbind(rep(1,68),rep(rev(0:1),34),rep(0:1,34))
Zmat=cbind(rep(rev(0:1),34),rep(0:1,34))
#Xmat
#Zmat












#############################################################  
seq<-c("c","b","a")
adhesive=c(rep(seq,7))
toy=sort(rep(seq(1:7),3))
press=c(67,71.9,72.2,67.5,68.8,66.4,76,82.6,74.5,72.7,
78.1,67.3,73.1,74.2,73.2,65.8,70.8,68.7,75.6,84.9,69.0)
length(toy)
length(adhesive)
length(press)                                                 
#using LME4
lmer(press ~ adhesive +(1|toy))                                 
#############################################################   
#LME4 Chapter 1 - Dyestuff Data
Dyestuff                
lmer(Yield ~ 1 + (1|Batch),Dyestuff)
Dyestuff2
lmer(Yield ~ 1 + (1|Batch),Dyestuff2)
############################################################# 
Penicillin
#diameter plate sample
lmer(diameter ~ 1 + (1|plate) + (1|sample) ,Penicillin)
############################################################# 
Pastes
xtabs(~cask+batch,Pastes)
Pastes <- within(Pastes, sample <- factor(batch:cask))
Pastes
##############################################################

#Run the following code only once
BA99.data<- read.csv(file="BA99.csv",head=FALSE,sep=",")
BA99.data=BA99.data[,-1]
#BA99.data<-BA99.data[,-1]
#colnames(BA99.data)<-c("j1","j2","j3","s1","s2","s3","r1","r2","r3")
#BA99.data

ob.js<-c(BA99.data[,1],BA99.data[,2],BA99.data[,3],
BA99.data[,7],BA99.data[,8],BA99.data[,9])

method<-c(rep("J",(3*85)),rep("S",(3*85)))
method=factor(method)
true<-c(rep("t",(6*85)))
true=factor(true)

seq2<-c(1:85)
subj=c(rep(seq2,6))                                                
subj=factor(subj)

repl<-method<-c(rep("1",85),rep("2",85),rep("3",85),
rep("1",85),rep("2",85),rep("3",85))



##############################################################
#usign packages LME4 and NLME
library(lme4)
library(nlme)
###############################################################
lm(ob.js~method)    #indicates bias

###############################################################

BA99<-data.frame(ob.js,true, method,subj,repl)          
#interaction.plot(method,subj,BA86.obs, las =1)      #doesnt work yet

fit1<-lme(ob.js ~ method, data =BA99, random = ~1|subj)
fit2<-lme(ob.js ~ method, data =BA99, random = ~1|subj/method)
fit3<-lme(ob.js ~ 1+ method, data =BA99, random = ~1|subj)
fit4<-lme(ob.js ~ 1+ method, data =BA99, random = ~ -1 + 1|subj )
fit5<-lme(ob.js ~  method, data =BA99, random = ~1|subj/repl/method)
fit6<-lme(ob.js ~  -1 - method, data =BA99, random = ~1|subj/repl/method)




