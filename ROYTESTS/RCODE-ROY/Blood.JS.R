#Blood JS Comparison
#
#
# Analysis under Bland and Altman
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

summary(JS)


dat=JS
JS.roy1 = lme(y ~ meth-1, data = dat,random = list(item=pdSymm(~ meth-1)), weights=varIdent(form=~1|meth),correlation = corSymm(form=~1 | item/repl), method="ML")
JS.roy2 = lme(y ~ meth-1, data = dat,random = list(item=pdCompSymm(~ meth-1)), correlation = corSymm(form=~1 | item/repl), method="ML")
JS.roy3 = lme(y ~ meth-1, data = dat,random = list(item=pdSymm(~ meth-1)),weights=varIdent(form=~1|meth), correlation = corCompSymm(form=~1 | item/repl), method="ML")
JS.roy4 = lme(y ~ meth-1, data = dat,random = list(item=pdCompSymm(~ meth-1)), correlation = corCompSymm(form=~1 | item/repl), method="ML")

getSigma(JS.roy1)
getOmega(JS.roy1)
roy.DV(JS.roy1)

#################################################################
# Analysis using BXC

JS.bxc1 = lme( y ~ meth + item, random = list( item = pdIdent( ~ meth-1 ) ), weights = varIdent( form = ~1 | meth ), data=dat)
JS.bxc2 = lme( y ~ meth + item, random=list( item = pdIdent( ~ meth-1 ), repl = ~1 ), weights = varIdent( form = ~1 | meth ), data=dat )
bxc.SDV(JS.bxc1)
bxc.SDV(JS.bxc2)
