X= c(47	,66	,68	,69	,70	,70	,73	,75	,79	,81	,
85	,87	,87	,87	,90	,100	,104	,105	,112	,120	,132)	

Y=c(43	,70	,72	,81	,60	,67	,72	,72	,92	,76	,85	,
82	,90	,96	,82	,100	,94	,98	,
108	,131	,131)	


fit1=lm(X~Y)
summary(fit1)

plot(X,Y, 
pch =17, 
col="red",
xlim=c(20,160),
ylim=c(20,160), ylab=expression(bold("Left ventricular stroke volume (SV) ")),
xlab=expression(bold("Transmitral volumetric flow (MF) ")),

main=c("Comparison of Deming and Linear Regression"),sub=c("Zhang et al 1986"),
)

abline(8.3060,0.9060,lty=1)
abline( -4.1216, 1.0451,lty=2)

temp <- legend("topleft", legend = c("Linear regression ", "Deming regression"),
               text.width = strwidth("Limits of agreement"),
               lty = 1:2, xjust = 1, yjust = 1,
              )


    
