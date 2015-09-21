options(digits = 5, show.signif.stars = FALSE)

# Packages: lattice, ellipse, xtable

        
#   -----   non-central t distribution   -------

NCT = function(DF = 10, NCP = 0, SIG = 0.05)
{
    qu = qt(SIG/2, DF, lower.tail = FALSE)
    pt(qu, DF, ncp = NCP, lower.tail = FALSE) + 
        pt(-qu, DF, ncp = NCP, lower.tail = TRUE)
}

#   -----   non-central F distribution   -------

NCF = function(DF1 = 1, DF2 = 8, NCP = 0, SIG = 0.05)
{
    qu = qf(SIG, DF1, DF2, lower.tail = FALSE)
    pf(qu, DF1, DF2, ncp = NCP, lower.tail = FALSE) 
}



#   -----   basic setup steps   -------

BIAS = seq(0,2.0,by=0.01)
TESTs = c("paired","regression")
Ns = c(10,15,20)
#LAMBDAs = c(1,3,0.1)
sigmaREs = c(5,1,0.5)
lambda = 1    # try 1,3,0.1   

#Nrows = length(BIAS) * length(TESTs) * length(Ns) * length(LAMBDAs)
Nrows = length(BIAS) * length(TESTs) * length(Ns) * length(sigmaREs)

PCs = data.frame( Power = seq(Nrows) , 
        Bias = rep(BIAS, length(TESTs) * length(Ns) * length(sigmaREs) ),
        Test = rep( rep( TESTs , rep( length(BIAS) , 2 ) ) , length(Ns) * length(sigmaREs) ), 
        N = ordered( rep( rep( Ns , rep( length(BIAS) * length(TESTs) , length(Ns) ) ), length(sigmaREs) ) ),
        #Lambda = ordered( rep( LAMBDAs , rep( length(BIAS) * length(TESTs) * length(Ns) , length(LAMBDAs) ) ) , levels = rev(LAMBDAs) ) )
        sigmaRE = ordered( rep( sigmaREs , rep( length(BIAS) * length(TESTs) * length(Ns) , length(sigmaREs) ) ) , levels = rev(sigmaREs) ) )


#   -----   do the calculations   -------

#for(i in 1:length(LAMBDAs) )
for(i in 1:length(sigmaREs) )
{
#    lambda.i = LAMBDAs[i]
    sigmaREs.i = sigmaREs[i]
    varDIFFs = 1 + (lambda)^2 
    varREG = varDIFFs -  (1 - (lambda)^2 )^2 / ( 4 * sigmaREs.i + varDIFFs ) 
    for(j in 1:length(Ns))
    {
        n.j = Ns[j]
        ncpNCT = sqrt( n.j / varDIFFs ) * BIAS
        ncpNCF = n.j * (BIAS^2) / varREG
        PCs[ ( (PCs[,5] == sigmaREs.i) & (PCs[,4] == n.j) ) & ( PCs[,3] == "paired") , 1 ] = 
            NCT(DF = (n.j -1), NCP = ncpNCT, SIG = 0.05)
        PCs[ ( (PCs[,5] == sigmaREs.i) & (PCs[,4] == n.j) ) & ( PCs[,3] == "regression") , 1 ] = 
            NCF(DF1 = 1, DF2 = (n.j -2), NCP = ncpNCF, SIG = 0.05)
    }
}


#   -----   plot the results   -------

fooNs = paste("c(", paste("expression(paste(", 
    rep("n==",3) , Ns, " ))", sep="", collapse=","), ")")
fooSIGs = paste("c(", paste("expression(paste(", 
    rep("sigma[tau]==",3) , rev(sigmaREs), " ))", sep="", collapse=","), ")") 

my.strip = function(which.given, ..., factor.levels) 
{
  levs = if (which.given == 1) factor.levels=eval(parse(text=fooNs))
           else factor.levels=eval(parse(text=fooSIGs))
  strip.default(which.given, ..., factor.levels = levs)
}


xyplot( Power ~ Bias | N*sigmaRE, data = PCs, groups = Test, 
        type="l" , lty = c(2,1) , strip = my.strip,
        ylab = expression(paste("Power curves    ",
            sigma[2]==1 ))) 



#   ----    Differences between power curves    ----    

PCdiffs = PCs
PCdiffs$Power[PCs$Test=="paired"] = (subset(PCs,
    subset=(Test=="regression"))$Power)-(subset(
        PCs,subset=(Test=="paired"))$Power)
PCdiffs = subset(PCdiffs,subset=(Test=="paired"))

xyplot( Power ~ Bias | N*sigmaRE, data = PCdiffs, groups = Test, 
        type="l" , lty = 1, col=1, strip = my.strip, 
        ylab= expression(paste("Differences between power curves    ",
            sigma[2]==1 ))) 


    #   for lambda = 1 calculate:
round( range( PCdiffs$Power[ (PCdiffs$N==10) ] ),3)
round( range( PCdiffs$Power[ (PCdiffs$N==15) ] ),3)
round( range( PCdiffs$Power[ (PCdiffs$N==20) ] ),3)





#   -----   Calculate the max diff between PCs when s1=s2    -----   

p0 = subset(PCs,subset=(Lambda==1))
p1 = subset(p0,subset=(Test=="paired"))$Power
p2 = subset(p0,subset=(Test=="regression"))$Power
hist(p1-p2)



#   -----   Rejection regions of joint and marginal tests    -----   

n=20
sig.level=0.05
plot.new()
plot.window(xlim=c(-3,3),ylim=c(-3,3))
axis(1,pos=0,at=c(-3.3,1,2,3),labels=rep("",4),tcl=0.3,cex.axis=0.8)
axis(2,pos=0,at=c(-3.3,1,2,3),labels=rep("",4),tcl=0.3,cex.axis=0.8,las=1)
text(rep(0.1,3),1:3,1:3,cex=0.7,pos=2)
text(1:3,rep(0.1,3),1:3,cex=0.7,pos=1)
mtext(expression(t[0]*{"*"}),side=3,las=1)
mtext(expression(t[1]*{"*"}),side=4,las=1)
radius=sqrt(2*qf(p=sig.level,df1=2,df2=(n-2),lower.tail=FALSE))
lines(ellipse(x=0,t=radius),type="l",col="red")
Tval=qt(p=sig.level/2,df=(n-2),lower.tail=FALSE)
segments((-Tval),(-Tval),(-Tval),Tval,lty=3,col="black")
segments((-Tval),(-Tval),Tval,(-Tval),lty=3,col="black")
segments(Tval,Tval,Tval,(-Tval),lty=3,col="black")
segments(Tval,Tval,(-Tval),Tval,lty=3,col="black")
Tval=qt((1-sqrt(0.95))/2,df=(n-2),lower.tail=FALSE)
segments((-Tval),(-Tval),(-Tval),Tval,lty=2,col="black")
segments((-Tval),(-Tval),Tval,(-Tval),lty=2,col="black")
segments(Tval,Tval,Tval,(-Tval),lty=2,col="black")
segments(Tval,Tval,(-Tval),Tval,lty=2,col="black")
arrows(0,0,(-radius/sqrt(2)),radius/sqrt(2),lty=1,col="black")
text(-1,1.2,"r")
    #   Let A and B be independent with Prob(A) = Prob(B) = p
    #   Select p so that p^2 = 0.95. Then sqrt(0.95) = 0.9746794



#   -----   Simultaneous inference    -----   

n=20
sig.level=0.05
plot.new()
plot.window(xlim=c(-3,3),ylim=c(-3,3))
axis(1,pos=0,at=c(-3.3,1,2,3),labels=rep("",4),tcl=0.3,cex.axis=0.8)
axis(2,pos=0,at=c(-3.3,1,2,3),labels=rep("",4),tcl=0.3,cex.axis=0.8,las=1)
text(rep(0.1,3),1:3,1:3,cex=0.7,pos=2)
text(1:3,rep(0.1,3),1:3,cex=0.7,pos=1)
mtext(expression(t[0]*{"*"}),side=3,las=1)
mtext(expression(t[1]*{"*"}),side=4,las=1)
radius=sqrt(2*qf(p=sig.level,df1=2,df2=(n-2),lower.tail=FALSE))
abline(h=c(-1,1)*radius,lty=2)
abline(v=c(-1,1)*radius,lty=2)
arrows(0,0,(-radius/sqrt(2)),radius/sqrt(2),lty=1,col="black")
text(-1,1.2,"r")
lines(ellipse(x=0,t=radius),type="l",col="red")
points(1.5,1.5,pch=16,cex=2)
points(2.9,2.9,pch=15,cex=2)
points(1.5,2.9,pch=17,cex=2)
points(2.9,1.5,pch=18,cex=2)
points(2.25,2.25,pch="+",cex=2)




#   -----   Confidence ellipse    -----   

n=20
sig.level=0.05
radius=sqrt(2*qf(p=sig.level,df1=2,df2=(n-2),lower.tail=FALSE))
#shifts=cbind(c(1,-1.2,1,-1,-0.4),c(1,-1.3,-1,1,-0.3))
shifts=cbind(c(1,-1,1,-0.4,-1.2),c(1,1,-1,-0.4,-1.3))
def.par=par(no.readonly = TRUE) # save default, for resetting...
layout(matrix(c(1,2,3,4,5,6), 2, 3, byrow = FALSE))
i=5
    plot.new()
    plot.window(xlim=c(-1,6),ylim=c(-1,6),asp=1)
    axis(1,pos=0,at=c(-1.6,1,3,5,7),labels=rep("",5),tcl=0.3,cex.axis=0.8)
    axis(2,pos=0,at=c(-1.3,1,3,5,7),labels=rep("",5),tcl=0.3,cex.axis=0.8,las=1)
    text(rep(0.1,3),c(1,3,5),c(1,3,5),cex=0.7,pos=2)
    text(c(1,3,5),rep(0.1,3),c(1,3,5),cex=0.7,pos=1)
    mtext(expression(t[0]),side=2,las=1)
    mtext(expression(t[1]),side=1,las=1)
    my.centre=c(radius,radius)+shifts[i,]
    lines(ellipse(x=0,t=radius,centre=my.centre),type="l",col="red")
    points(my.centre[1],my.centre[2],pch=16,cex=2)
style = c(15,17,18,19)
plot.new()
for(i in 1:4)
{
    plot.new()
    plot.window(xlim=c(-1,6),ylim=c(-1,6),asp=1)
    axis(1,pos=0,at=c(-1.6,1,3,5,7),labels=rep("",5),tcl=0.3,cex.axis=0.8)
    axis(2,pos=0,at=c(-1.3,1,3,5,7),labels=rep("",5),tcl=0.3,cex.axis=0.8,las=1)
    text(rep(0.1,3),c(1,3,5),c(1,3,5),cex=0.7,pos=2)
    text(c(1,3,5),rep(0.1,3),c(1,3,5),cex=0.7,pos=1)
    mtext(expression(t[0]),side=2,las=1)
    mtext(expression(t[1]),side=1,las=1)
    my.centre=c(radius,radius)+shifts[i,]
    lines(ellipse(x=0,t=radius,centre=my.centre),type="l",col="red")
    if(i <= 3) points(my.centre[1],my.centre[2],pch=style[i],cex=2)
    else points(my.centre[1],my.centre[2],pch="+",cex=2)
}


#   ----    Bartko (1994) eye tracking data     ----    

X = c(52,53,59,60,59,59,57,53,54)
Y = c(58,55,56,54,59,60,59,58,52)
Ds = (X - Y)
As = (X + Y)/2

plot( As , Ds , xlab = "Average of two methods", 
    ylab = "Difference between two methods" , xlim = c(50,63),
    ylim = c(-10,10), main = "Eye tracking data in milliseconds")
avgDs = mean(Ds); sdDs = sd(Ds)
abline( h=c(avgDs - 2*sdDs, avgDs + 2*sdDs), lty = 2, col = 3)
abline( h = avgDs , lty = 2, col = "grey")

xtable( round( rbind(X,Y,Ds,As),1) )

t.test(Ds)
cor.test(Ds,As, method = "pearson")

Cs = As - mean(As)
SLR = lm(Ds ~ Cs)
summary(SLR)


paste("Bradley-Blackwood test statistic:  ", 
    round(sum((summary(SLR)$coefficients[,3])^2)/2,3), 
    " (p = ", round(pf(q=sum((summary(SLR)$coefficients[,3])^2)/2 ,
        df1=2, df2=length(Ds)-2, lower.tail = FALSE ),4),")" )

round(abs(summary(SLR)$coefficients[,3]),3);
sqrt(2*pf(q=0.95, df1=2, df2=length(Ds)-2, lower.tail = FALSE ))


###################################################
# Bland JM, Altman DG. (1995) Comparing methods of 
# measurement: why plotting difference against standard 
# method is misleading. Lancet, 346, 1085-7. 
# Systolic blood pressure (mm Hg) measurements made using 
# a standard arm cuff and a finger monitor 


X = SystolicArm
Y = SystolicFinger
Ds = Y - X ; As = (Y + X)/2
avgDs = mean(Ds) ; sdDs = sd(Ds)
plot( As , Ds , xlab = "Average systolic pressure (mm Hg)", 
    ylab = "Finger - arm pressure" , 
    ylim = c(-60,60), xlim = c(50,250), 
    main = "Bland-Altman MCS plot")
abline( h = c(avgDs - 2*sdDs, avgDs + 2*sdDs), lty = 2, col = 3)
abline( h = avgDs , lty = 2, col = "grey")
abline( lm( Ds ~ As ) , col = 2, lwd =2)

Cs = As - mean(As)
SLR = lm( Ds ~ Cs )
summary(SLR)

paste("Bradley-Blackwood test statistic:  ", 
    round(sum((summary(SLR)$coefficients[,3])^2)/2,3), 
    " (p = ", round(pf(q=sum((summary(SLR)$coefficients[,3])^2)/2 ,
        df1=2, df2=length(Ds)-2, lower.tail = FALSE ),4),")" )

round(abs(summary(SLR)$coefficients[,3]),3);
sqrt(2*pf(q=0.95, df1=2, df2=length(Ds)-2, lower.tail = FALSE ))




# Bland JM, Altman DG. (1999) Measuring agreement 
# in method comparison studies. Statistical Methods 
# in Medical Research 8, 135-160. 
# Fat content of human milk determined by enzymic 
# procedure for the determination of triglycerides 
# and measured by the Standard Gerber method (g/100 ml).

Y = Trig
X = Gerber
Ds = Y - X ; As = (Y + X)/2
avgDs = mean(Ds) ; sdDs = sd(Ds)
plot( As , Ds , ylab = "Trig-Gerber differences  (g/100 ml)", 
    xlab = "Average fat content (g/100 ml)" , 
    ylim = c(-0.3,0.2), xlim = c(0,6.2), 
    main = "Fat content of human milk")
abline( h = c(avgDs - 2*sdDs, avgDs + 2*sdDs), lty = 2, col = 3)
abline( h = avgDs , lty = 2, col = "grey")
abline( lm( Ds ~ As ) , col = 2, lwd =2)

Cs = As - mean(As)
SLR = lm( Ds ~ Cs )
summary(SLR)

paste("Bradley-Blackwood test statistic:  ", 
    round(sum((summary(SLR)$coefficients[,3])^2)/2,3), 
    " (p = ", round(pf(q=sum((summary(SLR)$coefficients[,3])^2)/2 ,
        df1=2, df2=length(Ds)-2, lower.tail = FALSE ),4),")" )

round(abs(summary(SLR)$coefficients[,3]),3);
sqrt(2*pf(q=0.95, df1=2, df2=length(Ds)-2, lower.tail = FALSE ))


#  Data used by Student (1908) and available in R 
#  object "sleep". The data show the effect of two 
#  soporific drugs (increase in hours of sleep 
#  compared to control) on 10 patients. 

t.test(extra ~ group, data = sleep, paired = TRUE)

Dhh=subset(sleep,group==1)$extra
Lhh=subset(sleep,group==2)$extra
Ds = Lhh - Dhh
As = (Lhh + Dhh)/2
Cs = As - mean(As)

SLR = lm( Ds ~ Cs )
summary(SLR)

paste("Bradley-Blackwood test statistic:  ", 
    round(sum((summary(SLR)$coefficients[,3])^2)/2,3), 
    " (p = ", round(pf(q=sum((summary(SLR)$coefficients[,3])^2)/2 ,
        df1=2, df2=length(Ds)-2, lower.tail = FALSE ),4),")" )

round(abs(summary(SLR)$coefficients[,3]),3);
sqrt(2*pf(q=0.95, df1=2, df2=length(Ds)-2, lower.tail = FALSE ))

plot(As,Ds); cbind(As,Ds)









#   -----   verify the results using simulation -------
#   Old code, not really used inthis paper but I am slow
#   tp throw it away just yet...

myPCs = subset( PCs , subset = (N == 10) )
myPCs = subset( myPCs , subset = (Bias == 2.0) )
myPCs = subset( myPCs , subset = (Lambda == 3.0) )
myPCs

simMCSpair = function(n=10,muD=2,sigmaOne=1,sigmaTwo=3,sigmaREs=1)
{
    REs = rnorm(n,0,sigmaREs)
    Xs = rnorm(n,0,sigmaOne) + REs
    Ys = muD + rnorm(n,0,sigmaTwo) + REs
    Ds = Xs-Ys
    Ms = (Xs+Ys)/2
    Student = abs((sqrt(n)*mean(Ds))/sd(Ds)) > qt(p=0.975,df=(n-1))
    Zs = Ms - mean(Ms)
    SLR = lm( Ds ~ Zs )
    InterceptTest = summary(SLR)$coefficients[1,4] < 0.05
   return( as.numeric( c(Student,InterceptTest) ) )
}

simMCSpair()

TestResults = c(0,0)
K = 1000
for(k in 1:K)
{
    M = 10000
    simResults = matrix(0,M,2)
    for(i in 1:M)
    { 
        simResults[i,] = simMCSpair() 
    }
    TestResults = TestResults + apply( simResults , 2 , sum )
}

TestResults/(K*M)
myPCs


#> TestResults/(K*M)
#[1] 0.431328 0.628433
#> myPCs
#         Power Bias       Test  N Lambda  Nvals    LAMvals
#2613 0.4313263    2   marginal 10      3 n = 10 sigma2 = 3
#2814 0.6181687    2 regression 10      3 n = 10 sigma2 = 3

sqrt(  ( 0.6 * 0.4 ) / 1000000  )

#
