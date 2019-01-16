##### Calculate Power for Varying Sample Sizes
n.options = seq(2,20,by=1) 
res.power = NA

for(i in 1:length(n.options)) { 
  res = power.anova.test(groups=4, 
                         between.var=var(c(22, 23, 29, 30)), 
                         within.var = 25, 
                         sig.level=0.05, 
                         n=n.options[i])
  res.power[i] = res$power 
}
plot(n.options,
     res.power,
     type="l", 
     xlab="Sample Size", 
     ylab="Power", 
     main="Power Curve for Cookie Type Study")

# Part b
power.anova.test(groups=4, 
                 between.var=var(c(22, 23, 29, 30)), 
                 within.var = 25, 
                 sig.level=0.05, 
                 power = 0.85)

# Part c
n.options = seq(2,20,by=1) 
res.power = NA

for(i in 1:length(n.options)) { 
  res = power.anova.test(groups=4, 
                         between.var=var(c(30, 29, 23, 22)), 
                         within.var = 25, 
                         sig.level=0.05, 
                         n=n.options[i])
  res.power[i] = res$power 
}
plot(n.options,
     res.power,
     type="l", 
     xlab="Sample Size", 
     ylab="Power", 
     main="Power Curve for Cookie Type Study")


# Part d
n.options = seq(2,20,by=1) 
res.power = NA

for(i in 1:length(n.options)) { 
  res = power.anova.test(groups=4, 
                         between.var=var(c(21, 26, 26, 31)), 
                         within.var = 25, 
                         sig.level=0.05, 
                         n=n.options[i])
  res.power[i] = res$power 
}
plot(n.options,
     res.power,
     type="l", 
     xlab="Sample Size", 
     ylab="Power", 
     main="Power Curve for Cookie Type Study")
