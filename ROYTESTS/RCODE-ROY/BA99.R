

#BA99.data

#Run the following code only once
BA99.data<- read.csv(file="BA99.csv",head=FALSE,sep=",")
  #Run the following code only once - removes the unnecessary indices
BA99.data=BA99.data[,-1]

  #510 Observations (85 subjects 2 methods 3 Replicates )

ob.js<-c(BA99.data[,1],BA99.data[,2],BA99.data[,3],
BA99.data[,7],BA99.data[,8],BA99.data[,9])

  #using two methods
method<-c(rep("J",(3*85)),rep("S",(3*85)))
method=factor(method)
  #85 subjects
seq2<-c(1:85)
subj=c(rep(seq2,6))                                                
subj=factor(subj)
  #3 replicates on each subject

repl<-method<-c(rep("1",85),rep("2",85),rep("3",85),
rep("1",85),rep("2",85),rep("3",85))



##############################################################
#using packages LME4 and NLME
library(lme4)
library(nlme)




###############################################################
lm(ob.js~method)    #indicates bias

###############################################################
#Dataframe
BA99<-data.frame(ob.js, method,subj,repl)          




################################################################
#Fits

fit1<-lme(ob.js ~ method, data =BA99, random = ~1|subj)
fit2<-lme(ob.js ~ method, data =BA99, random = ~1|subj/method)
fit3<-lme(ob.js ~ 1+ method, data =BA99, random = ~1|subj)
fit4<-lme(ob.js ~ 1+ method, data =BA99, random = ~ -1 + 1|subj )
fit5<-lme(ob.js ~  method, data =BA99, random = ~1|subj/repl/method)
fit6<-lme(ob.js ~  -1 - method, data =BA99, random = ~1|subj/repl/method)


##############################################################################


# consider J and S groups only:
J.sd = c(with(subset(blood, subset = method == "J"), by(BP, subject, sd))) 
S.sd = c(with(subset(blood, subset = method == "S"), by(BP, subject, sd)))
min(J.sd) ; max(J.sd)
min(S.sd) ; max(S.sd)
plot(J.sd, S.sd)

# make a data frame containing J and S groups only:
dat = subset(blood, subset = method != "R")

# lines plot of cell means:
with(dat, interaction.plot(method, subject, BP, legend = FALSE))

################################################################################

fit1 = lme( BP ~ method, data = dat, random = ~1 | subject )
fit2 = update(fit1, random = ~1 | subject/method )
fit3 = update(fit1, random = ~method - 1 | subject )
#analysis of variance
anova(fit1,fit2,fit3)

################################################################################

fit1a = lme( BP ~ method, data = dat, random = pdDiag(~1 | subject ))
fit1b = lme( BP ~ method, data = dat, random = pdIdent(~1 | subject ))
fit1c = lme( BP ~ method, data = dat, random = pdSymm(~1 | subject ))
fit1d = lme( BP ~ method, data = dat, random = pdCompSymm(~1 | subject ))
anova(fit1,fit1a,fit1b,fit1c,fit1d)
################################################################################

fit2a = update(fit2, random = pdDiag(~1 | subject/method ))
fit2b = update(fit2, random = pdIdent(~1 | subject/method ))
fit2c = update(fit2, random = pdSymm(~1 | subject/method ))
fit2d  = update(fit2, random = pdCompSymm(~1 | subject/method ))

anova(fit2,fit2a,fit2b,fit2c,fit2d)
################################################################################

fit3a = update(fit3, random = pdDiag(~method - 1 | subject ))
fit3b = update(fit3, random = pdIdent(~method - 1 | subject ))
fit3c = update(fit3, random = pdSymm(~method - 1 | subject ))
fit3d = update(fit3, random = pdCompSymm(~method - 1 | subject ))
anova(fit3,fit3a,fit3b,fit3c,fit3d)
################################################################################


