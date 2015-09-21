#Roy and Carstensen analysis for Hamlett Data set

#
# Analysus under Bland and Altman
# Analysis under Roy's Method
#       - Implementation of the four models
#       - Expression of the Matrices
#       - Implementation of the three hypothesis tests        
# Analysis under BXC
#       - Computation of Limits of Agreement
#################################################################
#
# Load useful packages for analysis
library(MethComp)
library(nlme)

#################################################################
summary(hamlett) 



# Roys Model - unconstrained model
hamlett.roy1= lme(y ~ meth-1, data = hamlett, random = list(item=pdSymm(~ meth-1)),weights=varIdent(form=~1|meth), correlation = corSymm(form=~1 | item/repl), method="ML")

################################################################
getD(hamlett.roy1)
getSigma(hamlett.roy1)
getOmega(hamlett.roy1)
roy.SDV(hamlett.roy1)



###############################################################
dat = hamlett

#construction of 4 models for hypothesis tests.
hamlett.roy1 = lme(y ~ meth-1, data = dat,random = list(item=pdSymm(~ meth-1)), weights=varIdent(form=~1|meth),correlation = corSymm(form=~1 | item/repl), method="ML")
hamlett.roy2 = lme(y ~ meth-1, data = dat,random = list(item=pdCompSymm(~ meth-1)), correlation = corSymm(form=~1 | item/repl), method="ML")
hamlett.roy3 = lme(y ~ meth-1, data = dat,random = list(item=pdSymm(~ meth-1)),weights=varIdent(form=~1|meth), correlation = corCompSymm(form=~1 | item/repl), method="ML")
hamlett.roy4 = lme(y ~ meth-1, data = dat,random = list(item=pdCompSymm(~ meth-1)), correlation = corCompSymm(form=~1 | item/repl), method="ML")



#ANOVAs
test1 = anova(hamlett.roy1,hamlett.roy2) # Between-Subject Variabilities
test2 = anova(hamlett.roy1,hamlett.roy3) # Within-Subject Variabilities
test3 = anova(hamlett.roy1,hamlett.roy4) # Overall Variabilities


#################################################################
# Analysis using BXC

hamlett.bxc1 = lme( y ~ meth + item, random = list( item = pdIdent( ~ meth-1 ) ), weights = varIdent( form = ~1 | meth ), data=dat)
hamlett.bxc2 = lme( y ~ meth + item, random=list( item = pdIdent( ~ meth-1 ), repl = ~1 ), weights = varIdent( form = ~1 | meth ), data=dat )

bxc.SDV(hamlett.bxc1)
bxc.SDV(hamlett.bxc2)
