

#Roy's SJ comparison

dat2= rbind(dat[256:510,],rbind(dat[1:255,]))


library(nlme)

JS.o = lme(BP ~ method-1, data = dat,random = list(subject=pdSymm(~ method-1)),weights=varIdent(form=~1|method), correlation = corSymm(form=~1 | subject/obs), method="ML")
JS.o
SJ.o = lme(BP ~ method-1, data = dat2,random = list(subject=pdSymm(~ method-1)),weights=varIdent(form=~1|method), correlation = corSymm(form=~1 | subject/obs), method="ML")
SJ.o
dat3=orderdata(dat2,"J","S")
SJ.oa = lme(BP ~ method-1, data = dat3,random = list(subject=pdSymm(~ method-1)),weights=varIdent(form=~1|method), correlation = corSymm(form=~1 | subject/obs), method="ML")
SJ.oa





















#####################################################################
#The M2LLs
M2ll.1 =summary(MCS1)$logLik * -2
M2ll.2 =summary(MCS2)$logLik * -2
M2ll.3 =summary(MCS3)$logLik * -2
M2ll.4 =summary(MCS4)$logLik * -2

#The AICs
AIC.1 =summary(MCS1)$AIC 
AIC.2 =summary(MCS2)$AIC 
AIC.3 =summary(MCS3)$AIC 
AIC.4 =summary(MCS4)$AIC 

getD(MCS1)
getSig2(MCS1)

AIC.1-AIC.2
AIC.1-AIC.3
AIC.1-AIC.4


library(nlme)
fit5 = lme(BP ~ method, data = dat, random = list(subject=pdDiag(~ method)), method="ML")
fit5a = lme(BP ~ method, data = dat, random = ~ method|subject, method="ML")
fit6 = lme(BP ~ method, data = dat, random = list(subject=pdSymm(~ method),obs=pdCompSymm(~ method)), method="ML")
fit7 = lme(BP ~ method, data = dat, random = list(subject=pdCompSymm(~ method),obs=pdSymm(~ method)), method="ML")
fit8 = lme(BP ~ method, data = dat, random = list(subject=pdCompSymm(~ method),obs=pdCompSymm(~ method)), method="ML")

summary(fit5)$logLik * -2
summary(fit5a)$logLik * -2
summary(fit6)$logLik * -2
summary(fit7)$logLik * -2
summary(fit8)$logLik * -2

anova(fit5,fit6,fit7,fit8)


summary(fit5)$logLik * -2 
summary(fit5)$AIC
summary(fit5)$BIC
summary(fit6)$logLik * -2
summary(fit6)$AIC
summary(fit6)$BIC
summary(fit7)$logLik * -2
summary(fit7)$AIC
summary(fit7)$BIC
summary(fit8)$logLik * -2
summary(fit8)$AIC
summary(fit8)$BIC


library(nlme)
fit5 = lme(BP ~ method, data = dat, random = list(subject=pdDiag(~ method)), method="ML")
fit6 = lme(BP ~ method, data = dat, random = list(subject=pdSymm(~ method)), method="ML")
fit7 = lme(BP ~ method, data = dat, random = list(subject=pdCompSymm(~ method)), method="ML")
fit8 = lme(BP ~ method, data = dat, random = list(subject=pdIdent(~ method)), method="ML")






fm3 <- lme(
    BP ~ method,
    data = dat, 
    random = list(subject=pdSymm(~ method)), 
    correlation = corSymm(),
    , method="ML"
    )

fm4 = lme (BP ~ method, 
data=dat, 
random=~method|subject, 
method="ML", 
weights=varIdent(form=~method), 
corr=corSymm(, ~1|subject/obs)
)




##############################
fm3 <- lme(
    BP ~ method,
    data = dat, 
    random = list(subject=pdSymm(~ method)), 
    correlation = corSymm(,form=~1|subject/obs),
    , method="ML"
    )
    
summary(fm3)$logLik * -2 
summary(fm3)
VarCorr(fm3)

##############################
fm3 <- lme(
    BP ~ method-1,
    data = dat, 
    random = list(subject=pdSymm(~ method-1)), 
    correlation = corCompSymm(,form=~1|subject/obs),
    , method="ML"
    )
    
summary(fm3)$logLik * -2 
summary(fm3)
VarCorr(fm3)




library(nlme)
library(mgcv)
fm1 <- lme(
    y ~ as.factor(s) * as.factor(trt), 
    data = rats, 
    random = ~ 1|id, 
    correlation = corCompSymm())
summary(fm1)

library(mgcv)
fm1 <- lme(
    BP ~ method,
    data = dat, 
    random = list(subject=pdSymm(~ method)), 
    correlation = corCompSymm(),
    , method="ML"
    )
    
summary(fm1)$logLik * -2 
summary(fm1)
VarCorr(fm1)

##############################
fm2 <- lme(
    BP ~ method,
    data = dat, 
    random = list(subject=pdSymm(~ method)), 
    correlation = corSymm(),
    , method="ML"
    )
    
summary(fm2)$logLik * -2 
summary(fm2)
VarCorr(fm2)
##############################
fm3 <- lme(
    BP ~ method,
    data = dat, 
    random = list(subject=pdSymm(~ method)), 
    correlation = corSymm(),
    , method="ML"
    )
    
summary(fm3)$logLik * -2 
summary(fm3)
VarCorr(fm3)
