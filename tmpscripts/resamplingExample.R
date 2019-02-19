source("../R/Module_Sub_boots_functions.R")

#for vector output from bootsrapping
#output has same length as input vector:
resampleSeries(x = rnorm(40))


#when only have the distribution stats:
mean.val <- 10000
pi.upper <- 20000
pi.level <- 0.9 #the upper prediction level, i.e for 80% PIs (0.1 to 0.9)

sd <- (pi.upper-mean.val)/qnorm(pi.level)

x.density <- resamplePDFdensity(distribution = qnorm, args = list(mean=mean.val, sd=sd), n = 2000)
hist(x.density)

x.rand <- resamplePDFrandom(distribution = rnorm, args = list(n=2000, mean=mean.val, sd=sd))
hist(x.rand)
