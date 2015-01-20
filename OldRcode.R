Y=c(35.6,47.9,34.2,31.2,49.5,30.5,32.7,49.3,31.0,28.7)
mean(Y);sd(Y);
median(Y)


Z = c(3,2,6,5,40,2,9,3,5,3,8,6,4,8,8,7,5,10,15,9,2,1,8,4,7,1,17,2,6,8,3,15,5,3,5,3,11,5,16,20)


blood = groupedData( y ~ meth | item ,
    data = data.frame( y = c(Blood), item = c(row(Blood)),
        meth = rep(c("J","R","S"), rep(nrow(Blood)*3, 3)),
        repl = rep(rep(c(1:3), rep(nrow(Blood), 3)), 3) ),
    labels = list(y = "Systolic Blood Pressure", meth= "Measurement Device"),
    order.groups = FALSE )

# make a data frame containing J and S groups only:
datJS = subset(blood, subset = meth != "R")
datRS = subset(blood, subset = meth != "J")
datJR = subset(blood, subset = meth != "S")

datJS$item <- factor(datJS$item)
datJS$repl <- factor(datJS$repl)

lme( y ~ meth + item,
 random = list( item = pdIdent( ~ meth-1 ) ),
 weights = varIdent( form = ~1 | meth ),
 data=datJS
)
lme( y ~ meth + item,
 random=list( item = pdIdent( ~ meth-1 ),
 repl = ~1 ),
 weights = varIdent( form = ~1 | meth ),
 data=datJS
)




datRS$item <- factor(datRS$item)
datRS$repl <- factor(datRS$repl)

lme( y ~ meth + item,
 random = list( item = pdIdent( ~ meth-1 ) ),
 weights = varIdent( form = ~1 | meth ),
 data=datRS
)
lme( y ~ meth + item,
 random=list( item = pdIdent( ~ meth-1 ),
 repl = ~1 ),
 weights = varIdent( form = ~1 | meth ),
 data=datRS
)






datJR$item <- factor(datJR$item)
datJR$repl <- factor(datJR$repl)

lme( y ~ meth + item,
 random = list( item = pdIdent( ~ meth-1 ) ),
 weights = varIdent( form = ~1 | meth ),
 data=datJR
)
lme( y ~ meth + item,
 random=list( item = pdIdent( ~ meth-1 ),
 repl = ~1 ),
 weights = varIdent( form = ~1 | meth ),
 data=datJR
)






data( fat )
fat <- data.frame( item=factor(fat$Id),
meth=fat$Obs,
repl=factor(fat$Rep),
y=fat$Sub )


lme( y ~ meth + item,
 random = list( item = pdIdent( ~ meth-1 ) ),
 weights = varIdent( form = ~1 | meth ),
 data=fat
)
lme( y ~ meth + item,
 random=list( item = pdIdent( ~ meth-1 ),
 repl = ~1 ),
 weights = varIdent( form = ~1 | meth ),
 data=fat
)





MCS1 = lme(y ~ meth-1, data = fat,
random = list(item=pdSymm(~ meth-1)),
weights=varIdent(form=~1|meth), correlation = corSymm(form=~1 | item/repl), method="ML")



MCS2 = lme(BP ~ method-1, data = datRS,
random = list(subject=pdSymm(~ method-1)),
weights=varIdent(form=~1|method), correlation = corSymm(form=~1 | subject/obs), method="ML")


MCS3 = lme(BP ~ method-1, data = datJR,
random = list(subject=pdSymm(~ method-1)),
weights=varIdent(form=~1|method), correlation = corSymm(form=~1 | subject/obs), method="ML")


MCS3 = lme(BP ~ method-1, data = ox,
random = list(subject=pdSymm(~ method-1)),
weights=varIdent(form=~1|method), correlation = corSymm(form=~1 | subject/obs), method="ML")



AP=c(
16,18,18,19,20,20,20,21,21,21,
22,25,25,26,26,26,26,28,30,31,
32,32,32,32,32,35,35,36,26,37,
38,44,46,48,51,52,52,53,59,64)

boxplot(AP, horizontal =TRUE)







lme( y ~ meth + item,
 random=list( item = pdIdent( ~ meth-1 ),
 repl = ~1 ),
 weights = varIdent( form = ~1 | meth ),
 data=datJS
)





MCS1 = lme(y ~ meth-1, data = fat,random = list(item=pdSymm(~ method-1)),weights=varIdent(form=~1|method), correlation = corSymm(form=~1 | subject/obs), method="ML")





fat <- data.frame( item=factor(fat$Id),
meth=fat$Obs,
repl=factor(fat$Rep),
y=fat$Sub )





lme( y ~ meth + item,
 random = list( item = pdIdent( ~ meth-1 ) ),
 weights = varIdent( form = ~1 | meth ),
 data=cardiac
)
lme( y ~ meth + item,
 random=list( item = pdIdent( ~ meth-1 ),
 repl = ~1 ),
 weights = varIdent( form = ~1 | meth ),
 data=cardiac
)




MCS1 = lme(y ~ meth-1, data = hamlett,
random = list(item=pdSymm(~ meth-1)),
weights=varIdent(form=~1|meth), correlation = corSymm(form=~1 | item/repl), method="ML")

lme( y ~ meth + item,
 random = list( item = pdIdent( ~ meth-1 ) ),
 weights = varIdent( form = ~1 | meth ),
 data=hamlett
)
lme( y ~ meth + item,
 random=list( item = pdIdent( ~ meth-1 ),
 repl = ~1 ),
 weights = varIdent( form = ~1 | meth ),
 data=hamlett
)




MCS2 = lme(y ~ meth-1, data = hamlett,
random=list( item = pdSymm( ~ meth-1 ),
 repl = ~1 ),
weights=varIdent(form=~1|meth), correlation = corSymm(form=~1 | item/repl), method="ML")


lme( y ~ meth + item,
 random=list( item = pdIdent( ~ meth-1 ),
 repl = ~1 ),
 weights = varIdent( form = ~1 | meth ),
 data=hamlett
)






getD = function(x) {
    v = as.numeric(VarCorr(x)[1:2,1])
    r = as.numeric(VarCorr(x)[2,3])
    Dhat = matrix(0,2,2)
    diag(Dhat) = v
    Dhat[1,2] = Dhat[2,1] = r * sqrt(prod(v))
    arr=rownames(summary(x)$contrasts$method)
    colnames(Dhat)=arr
    rownames(Dhat)=arr
    Dhat
}
#########################################################################


getSigma = function(x) {
    res = as.numeric(VarCorr(x)[6])
    V = max(is.null(intervals(x)$varStruct[2]),intervals(x)$varStruct[2])
    C = intervals(x)$corStruct[2]
    Sighat = matrix(1,2,2)
    Sighat[2,2] = as.numeric(VarCorr(x)[3,1])
    Sighat[1,1] = as.numeric(VarCorr(x)[3,1])*(V^2)
    Sighat[1,2] = Sighat[2,1] = as.numeric(VarCorr(x)[3,1])*V*C
    arr=rownames(summary(x)$contrasts$method)
    colnames(Sighat)=arr
    rownames(Sighat)=arr
    Sighat
}

#########################################################################
getOmega = function(x) {
    Omega = matrix(1,2,2)
    Omega[1,1] = getD(x)[1,1] + getSigma(x)[1,1] 
    Omega[2,2] = getD(x)[2,2] + getSigma(x)[2,2]
    Omega[1,2] = Omega[2,1] = getD(x)[1,2] + getSigma(x)[1,2]
    Omega
}


Roy.JS.1 = lme(y ~ meth-1, data = datJS,
random=list( item = pdSymm( ~ meth-1 ) ),
weights=varIdent(form=~1|meth), correlation = corSymm(form=~1 | item/repl), method="ML")


Roy.RS.1 = lme(y ~ meth-1, data = datRS,
random=list( item = pdSymm( ~ meth-1 ) ),
weights=varIdent(form=~1|meth), correlation = corSymm(form=~1 | item/repl), method="ML")

Roy.JR.1 = lme(y ~ meth-1, data = datJR,
random=list( item = pdSymm( ~ meth-1 ) ),
weights=varIdent(form=~1|meth), correlation = corSymm(form=~1 | item/repl), method="ML")

Roy.ox.1 = lme(y ~ meth-1, data = ox,
random=list( item = pdSymm( ~ meth-1 ) ),
weights=varIdent(form=~1|meth), correlation = corSymm(form=~1 | item/repl), method="ML")

Roy.fat.1 = lme(y ~ meth-1, data = fat,
random=list( item = pdSymm( ~ meth-1 ) ),
weights=varIdent(form=~1|meth), correlation = corSymm(form=~1 | item/repl), method="ML")

Roy.hamlett.1 = lme(y ~ meth-1, data = hamlett,
random=list( item = pdSymm( ~ meth-1 ) ),
weights=varIdent(form=~1|meth), correlation = corSymm(form=~1 | item/repl), method="ML")


getSigma(Roy.JS.1 )
getSigma(Roy.RS.1 )
getSigma(Roy.JR.1 )
getSigma(Roy.fat.1 )
getSigma(Roy.ox.1 )
getSigma(Roy.hamlett.1 )


getOmega(Roy.JS.1 )
getOmega(Roy.RS.1 )
getOmega(Roy.JR.1 )
getOmega(Roy.fat.1 )
getOmega(Roy.ox.1 )
getOmega(Roy.hamlett.1 )



getD(Roy.fat.1 )
getD(Roy.ox.1 )



Roy.card.1 = lme(y ~ meth-1, data = cardiac,
random=list( item = pdSymm( ~ meth-1 ) ),
weights=varIdent(form=~1|meth), correlation = corSymm(form=~1 | item/repl), method="ML")

getD(Roy.card.1)
getSigma(Roy.card.1)




############################################################################################################################



Bias = lme( BP ~ method  , data = dat , 
method="ML" ,  random = list( subject = pdSymm( ~ method-1  ) ) ,  
weights = varIdent( form = ~1 | method ) ,  
correlation = corSymm( form = ~1 | subject/obs ) )

fitA = lme( BP ~ method-1  , data = dat , 
method="ML" ,  random = list( subject = pdSymm( ~ method-1  ) ) ,  
weights = varIdent( form = ~1 | method ) ,  
correlation = corSymm( form = ~1 | subject/obs ) )

fitB = lme( BP ~ method-1  , data = dat , 
method="ML" ,  
random = list( subject = pdCompSymm( ~ method-1  ) ) ,  
weights = varIdent( form = ~1 | method ) ,  
correlation = corSymm( form = ~1 | subject/obs ) )

fitC = lme( BP ~ method-1  , data = dat , 
method="ML" ,  
random = list( subject = pdSymm( ~ method-1 ) ) ,  
#weights = varIdent( form = ~1 | method ) ,  
correlation = corCompSymm( form = ~1 | subject/obs ) )

fitD = lme( BP ~ method-1, data = dat , 
method="ML" ,  
random = list( subject = pdCompSymm( ~ method-1 ) ) ,  
#weights = varIdent( form = ~1 | method ) ,  
correlation = corCompSymm( form = ~1 | subject/obs ) )


Bias = lme( BP ~ method  , data = dat , 
method="ML" ,  random = list( subject = pdSymm( ~ method-1  ) ) ,  
weights = varIdent( form = ~1 | method ) ,  
correlation = corSymm( form = ~1 | subject/obs ) )


fitBBIAS = lme( BP ~ method  , data = dat , 
method="ML" ,  
random = list( subject = pdCompSymm( ~ method-1  ) ) ,  
weights = varIdent( form = ~1 | method ) ,  
correlation = corSymm( form = ~1 | subject/obs ) )


fitCBIAS = lme( BP ~ method  , data = dat , 
method="ML" ,  
random = list( subject = pdSymm( ~ method-1 ) ) ,  
#weights = varIdent( form = ~1 | method ) ,  
correlation = corCompSymm( form = ~1 | subject/obs ) )

fitDBIAS = lme( BP ~ method, data = dat , 
method="ML" ,  
random = list( subject = pdCompSymm( ~ method-1 ) ) ,  
#weights = varIdent( form = ~1 | method ) ,  
correlation = corCompSymm( form = ~1 | subject/obs ) )





















Roy.PEFR.1 = lme(y ~ meth-1, data = PEFR,
random=list( item = pdSymm( ~ meth-1 ) ),
weights=varIdent(form=~1|meth), correlation = corSymm(form=~1 | item/repl), method="ML")


lme( y ~ meth + item,
 random = list( item = pdIdent( ~ meth-1 ) ),
 weights = varIdent( form = ~1 | meth ),
 data=PEFR
)
lme( y ~ meth + item,
 random=list( item = pdIdent( ~ meth-1 ),
 repl = ~1 ),
 weights = varIdent( form = ~1 | meth ),
 data=PEFR
)
















lme( y ~ meth + item,
 random=list( item = pdIdent( ~ meth-1 ),
 repl = ~1 ),
 weights = varIdent( form = ~1 | meth ),
 data=datRS
)


data( ox )
ox$item <- factor(ox$item)
ox$repl <- factor(ox$repl)


lme( y ~ meth + item,
 random = list( item = pdIdent( ~ meth-1 ) ),
 weights = varIdent( form = ~1 | meth ),
 data=ox
)
lme( y ~ meth + item,
 random=list( item = pdIdent( ~ meth-1 ),
 repl = ~1 ),
 weights = varIdent( form = ~1 | meth ),
 data=ox
)
