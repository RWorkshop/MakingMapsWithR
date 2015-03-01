library(nlme)


head(Machines)
obs=c(rep(1:3,18))





machdat = groupedData( score ~ machine | worker ,
    data = data.frame
        ( 
        score = Machines$score,
        mach = Machines$Machine,
        worker = Machines$Worker,
        obs=c(rep(1:3,18))),
    labels = list(score = "Score", machine = "Machines"),
    order.groups = FALSE )

mach1 = lme(score ~ mach-1, data = machdat,random = list(worker=pdSymm(~ mach-1)), correlation = corSymm(form=~1 | worker/obs), method="ML")
summary(mach1)
VarCorr(mach1)

mach2 = lme(score ~ mach-1, data = machdat,random = list(worker=pdSymm(~ mach-1)), correlation = corCompSymm(form=~1 | worker/obs), method="ML")
summary(mach2)
VarCorr(mach2)

mach3 = lme(score ~ mach-1, data = machdat,random = list(worker=pdCompSymm(~ mach-1)), correlation = corSymm(form=~1 | worker/obs), method="ML")
summary(mach3)
VarCorr(mach3)

mach4 = lme(score ~ mach-1, data = machdat,random = list(worker=pdCompSymm(~ mach-1)), correlation = corCompSymm(form=~1 | worker/obs), method="ML")
summary(mach4)
VarCorr(mach4)

mach5 = lme(score ~ mach-1, data = machdat,random = list(worker=pdCompSymm(~ mach-1)), correlation = corAR1(form=~1 | worker/obs), method="ML")
summary(mach5)
VarCorr(mach5)

mach6 = lme(score ~ mach-1, data = machdat,random = list(worker=pdCompSymm(~ mach-1)), weights = varPower(form=~mach),correlation = corSymm(form=~1 | worker/obs), method="ML")
summary(mach6)
VarCorr(mach6)
machA = lme(score ~ mach-1, data = machdat,random = ~ mach-1, method="ML")
summary(machA)
VarCorr(machA)
