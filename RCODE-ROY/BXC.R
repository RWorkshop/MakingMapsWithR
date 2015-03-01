
 


MCS1 = lme(
BP ~ method-1, 
data = dat,
random = list(subject=pdSymm(~ method-1)),
weights=varIdent(form=~1|method), 
correlation = corSymm(form=~1 | subject/obs), 
method="ML")

roy = lme(
BP ~ method-1, 
data = dat,
random = list(subject=pdSymm(~ method-1),obs=pdSymm(~1)),
weights=varIdent(form=~1|method), 
correlation = corSymm(form=~1 | subject/obs), 
method="ML")


MCS1B = lme(
BP ~ method-1, 
data = dat,
random = list(subject=pdSymm(~ method-1),obs=pdSymm(~1)),
weights=varIdent(form=~1|method), 
correlation = corCompSymm(form=~1 | subject/obs), 
method="ML")

summary(MCS1A)
summary(MCS1B)


MCS2 = lme(
BP ~ method-1, 
data = dat,
random = list(subject=pdSymm(~ method-1)),
weights=varIdent(form=~1|method), 
correlation = corCompSymm(form=~1 | subject/obs), 
method="ML")
