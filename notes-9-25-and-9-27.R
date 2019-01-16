# Notes from 9/25/18
# Calculate the residuals
oreosSUB$resids <- resid(aov(milkabsgram~treatment, data=oreosSUB))
head(oreosSUB)

# Make the index plot to check independence
plot(oreosSUB$resids, type = "b")
abline(h = 0)

# Make normal qq plot
qqnorm(oreosSUB$resids)
qqline(oreosSUB$resids)

# to get the t-test for unequal variances
# specify var.equal=FALSE, difference in results is df
# Let's do this for oreo data BUT it is not necessary/appropriate
t.test(milkabsgram~treatment, data=oreosSUB, var.equal=FALSE)
t.test(milkabsgram~treatment, data=oreosSUB, var.equal=TRUE)

# Notes from 9/27/2018

# calculate the power from our study
power.t.test(n=20, sig.lev=.05, sd=.25, delta=.7735)

# calculate sample size needed for a similar oreo study
power.t.test(sig.level = .05, sd=.25, delta=.7735, power=.9)
