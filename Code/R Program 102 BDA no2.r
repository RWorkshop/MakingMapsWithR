mc.brugs <-mcmc.list.bugs(m1.brugs)

summary(mc.brugs)
print( xyplot(mc.brugs,main="BRugs"))
print( densityplot(mc.brugs))







