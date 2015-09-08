
library(nlme)

WrightOne = c(494, 395, 516, 434, 476, 557, 413, 442, 650,  433, 417, 656, 267, 478, 178, 423, 427)
WrightTwo = c(490, 397, 512, 401, 470, 611, 415, 431, 638,  429, 420, 633, 275, 492, 165, 372, 421)
MiniOne = c(512, 430, 520, 428, 500, 600, 364, 380, 658,  445, 432, 626, 260, 477, 259, 350, 451)
MiniTwo = c(525, 415, 508, 444, 500, 625, 460, 390, 642,  432, 420, 605, 227, 467, 268, 370, 443)

PEFR=c(MiniOne,MiniTwo,WrightOne,WrightTwo)
Subject= rep(1:17,4)
repl= c(rep(1,17),rep(2,17),rep(1,17),rep(2,17))
meth=c(rep("M",34),rep("W",34))

Flow = groupedData( resp ~ method | subject ,
        data = data.frame( resp = PEFR, subject = Subject,
        method = meth,
        obs = repl),
    labels = list(resp = "PEFR", method = "Measurement Device"),
    order.groups = FALSE )

H.o = lme(resp ~ method-1, data = Flow,random = list(subject=pdSymm(~ method-1)),weights=varIdent(form=~1|method), correlation = corSymm(form=~1 | subject/obs), method="ML")

######################################## Flow1
PEFR1=c(WrightOne,WrightTwo,MiniOne,MiniTwo)
Subject= rep(1:17,4)
repl= c(rep(1,17),rep(2,17),rep(1,17),rep(2,17))
meth1=c(rep("W",34),rep("M",34))

Flow1 = groupedData( resp ~ method | subject ,
        data = data.frame( resp = PEFR1, subject = Subject,
        method = meth1,
        obs = repl),
    labels = list(resp = "PEFR", method = "Measurement Device"),
    order.groups = FALSE )

H.oa = lme(resp ~ method-1, data = Flow1,random = list(subject=pdSymm(~ method-1)),weights=varIdent(form=~1|method), correlation = corSymm(form=~1 | subject/obs), method="ML")
######################################## Flow1(ordered)
Flow2=orderdata(Flow1)
H.ob = lme(resp ~ method-1, data = Flow2,random = list(subject=pdSymm(~ method-1)),weights=varIdent(form=~1|method), correlation = corSymm(form=~1 | subject/obs), method="ML")

########################################
getD(H.o)
getD(H.oa)
getD(H.ob) # accords with H.o

getSigma(H.o)
getSigma(H.oa)
getSigma(H.ob)
########################################
