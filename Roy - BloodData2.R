options(digits = 5, show.signif.stars = FALSE)
library(nlme)


blood = groupedData( BP ~ method | subject ,
    data = data.frame( BP = c(Blood), subject = c(row(Blood)),
        method = rep(c("J","R","S"), rep(nrow(Blood)*3, 3)),
        obs = rep(rep(c(1:3), rep(nrow(Blood), 3)), 3) ),
    labels = list(BP = "Systolic Blood Pressure", method = "Measurement Device"),
    order.groups = FALSE )


# consider J and S groups only:
J.sd = c(with(subset(blood, subset = method == "J"), by(BP, subject, sd))) 
S.sd = c(with(subset(blood, subset = method == "S"), by(BP, subject, sd)))
min(J.sd) ; max(J.sd)
min(S.sd) ; max(S.sd)
plot(J.sd, S.sd)

# make a data frame containing J and S groups only:
dat = subset(blood, subset = method != "R")
dat2= rbind(dat[256:510,],rbind(dat[1:255,]))

# lines plot of cell means:
with(dat, interaction.plot(method, subject, BP, legend = FALSE))


fit1 = lme( BP ~ method, data = dat, random = ~1 | subject )
fit2 = update(fit1, random = ~1 | subject/method )
fit3 = update(fit1, random = ~method - 1 | subject )
fit4 = lme( BP ~ method, data = dat, random = pdSymm(~method) ) 
anova(fit1, fit2, fit3, fit4)


lme4.1<-lmer(BP~method + (method|subject),dat,verbose = TRUE, REML=FALSE)
