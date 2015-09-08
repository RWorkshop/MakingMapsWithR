#Roy's Candidate Models

################################################################################
library(nlme)

#subsetting and ordering the 'blood' data

dat = subset(blood, subset = method != "R")

################################################################################
# Test 1 : Test For inter-method bias

Bias.LME = lme(BP ~ method+1, data = dat,
random = list(subject=pdSymm(~ method+1)),weights=varIdent(form=~1|method), correlation = corSymm(form=~1 | subject/obs), method="ML")

summary(Bias.LME)$tTable


################################################################################
# Test 2 to 4 - Variability tests

MCS1 = lme(BP ~ method-1, data = dat,random = 
list(subject=pdSymm(~ method-1)),
weights=varIdent(form=~1|method), 
correlation = corAR1(form=~1 | subject/obs), method="ML")

MCS2 = lme(BP ~ method-1, data = dat,random = 
list(subject=pdCompSymm(~ method-1)),weights=varIdent(form=~1|method), correlation = corSymm(form=~1 | subject/obs), method="ML")

MCS3 = lme(BP ~ method-1, data = dat,random = 
list(subject=pdSymm(~ method-1)), correlation = corCompSymm(form=~1 | subject/obs), method="ML")

MCS4 = lme(BP ~ method-1, data = dat,random = 
list(subject=pdCompSymm(~ method-1)), correlation = corCompSymm(form=~1 | subject/obs), method="ML")

# Three Variability Tests are carried out by using the ANOVA procedure

testA = anova(MCS1,MCS2) # Between-Subject Variabilities
testB = anova(MCS1,MCS3) # Within-Subject Variabilities
testC = anova(MCS1,MCS4) # Overall Variabilities

testA
testB
testC

##################################################################################

getD(MCS1)
getSigma(MCS1)
BlockOmega(MCS1)
getOmegaCorr(MCS1)
