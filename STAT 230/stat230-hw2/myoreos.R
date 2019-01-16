# The Art of Oreo Dunking
# Sam Tenney
# Finds which type of oreo absorbs more milk 

# Read in the data
oreos <- read.table(text="run,experiment,treatment,weightBefore,weightAfter
                   1,8,TJ,19.69,24.69
                   2,12,DS,20.36,24.43
                   3,11,DS,19.92,24.96
                   4,6,TJ,18.20,23.61
                   5,4,Reg,16.17,23.43
                   6,3,Reg,16.62,22.09
                   7,9,DS,20.31,25.92
                   8,1,Reg,16.27,22.77
                   9,10,DS,20.14,26.00
                   10,7,TJ,18.97,23.54
                   11,5,TJ,18.56,22.96
                   12,2,Reg,16.66,22.15", header = TRUE, sep=",")

# Part d - Provides a summary of the data read in previously
head(oreos)
tail(oreos)
str(oreos)

# Part e - Subsets the oreo dataset into the Regular and Trader Joe's treatments
oreosSUB <- subset(oreos, treatment %in% c("Reg", "TJ"))
oreosSUB$treatment <- droplevels(oreosSUB$treatment)
str(oreosSUB$treatment)

# Part f - Calculate the mean and standard dev. of amount of milk absorbed in 10 seconds for Reg and TJ Oreos
oreosSUB$milkabsgram <- oreosSUB$weightAfter - oreosSUB$weightBefore


#Calculate mean and sd for each cookie type
aggregate(milkabsgram~treatment, data = oreosSUB, FUN = mean)
aggregate(milkabsgram~treatment, data = oreosSUB, FUN = sd)

# Part g - Boxplots of the amount milk absorbed in 10 seconds for Regular Oreos and Trader Joe's Joe-Joe's
boxplot(milkabsgram~treatment, data=oreosSUB, ylab="Milk Absorbed in 10 Seconds (grams)")

# Part h - Dotplots of the amount of milk absorbed in 10 seconds
library(lattice)
dotplot(milkabsgram~treatment, data=oreosSUB, ylab="Milk Absorbed in 10 Seconds (grams)")

# Homework 3;
oreos$milkabsgram <- oreos$weightAfter - oreos$weightBefore

# Mean for Regular Oreos
regMean <- mean(oreos$milkabsgram[oreos$treatment == "Reg"])
regMean

# Standard deviation for Regular  Oreos
Regsd <- sd(oreos$milkabsgram[oreos$treatment == "Reg"])
Regsd

# Sample size for Regular Oreos
Regsize <- length(oreos$milkabsgram[oreos$treatment == "Reg"])
Regsize

# Calculate 95% Confidence Interval for mean amount of milk absorbed for Regular Oreos
regMean + qt(.975, df=(Regsize-1))*Regsd/sqrt(Regsize)
regMean - qt(.975, df=(Regsize-1))*Regsd/sqrt(Regsize)

# Mean for Trader Joe's Oreos
TJmean <- mean(oreos$milkabsgram[oreos$treatment == "TJ"])
TJmean

# Standard deviation for Trader Joe's Oreos
TJsd <- sd(oreos$milkabsgram[oreos$treatment == "TJ"])
TJsd

# Sample size for Trader Joe's Oreos
TJsize <- length(oreos$milkabsgram[oreos$treatment == "TJ"])
TJsize

# Calculate 95% Confidence Interval for mean amount of milk absorbed for Trader Joe's Oreos
TJmean + qt(.975, df=(TJsize-1))*TJsd/sqrt(TJsize)
TJmean - qt(.975, df=(TJsize-1))*TJsd/sqrt(TJsize)

# Ratio to check if variance is constant
Regsd / TJsd

# Part d - Identify variables 
n1 <- Regsize
n2 <- TJsize
y14 <- oreosSUB$milkabsgram[oreosSUB$treatment == "Reg"][4]
y23 <- oreosSUB$milkabsgram[oreosSUB$treatment == "TJ"][3]
error14 <- y14 - regMean
error23 <- y23 - TJmean

# Part e - Conduct a two-sample t-test by hand
sp2 <- ((n1 - 1) * Regsd^2 + (n2 - 1) * TJsd^2)/(n1 + n2 - 2)
teststat <- (regMean - TJmean - 0)/(sqrt(sp2/n1 + sp2/n2))
teststat
p_value <- 2 * (1 - pt(abs(teststat), df = n1 + n2 - 2))
p_value

# Part f - Conduct a two-sample t-test using R
t.test(milkabsgram~treatment, data = oreosSUB, var.equal = TRUE)

# Part h - Calculate a confidence interval for the difference in mean amount of milk absorbed in 10 seconds
mean_diff <- regMean - TJmean

# Homework 4

# Part b - Calculate the residuals and find their mean and standard deviation
oreosSUB$resids <- resid(aov(milkabsgram~treatment, data=oreosSUB))
mean(oreosSUB$resids)
sd(oreosSUB$resids)

# Part c - Create a plot of residuals vs the order (index plot)
plot(oreosSUB$resids, type = "b", main = "Residuals vs. Order Collected", ylab = "Residuals")
abline(h = 0)

# Part d - Create a normal qqplot
qqnorm(oreosSUB$resids, main = "Normal QQ Plot of Residuals")
qqline(oreosSUB$resids)

# Part 6a
power.t.test(n = 4, sig.level = .05, delta = mean_diff, sd = sp2)

# Part 6b
power.t.test(sig.level = .05, delta = mean_diff, power = .80, sd = sp2)

# Homework 6

# Part b
# Means for each cookie type
regMean <- mean(oreos$milkabsgram[oreos$treatment == "Reg"])
TJmean <- mean(oreos$milkabsgram[oreos$treatment == "TJ"])
DSMean <- mean(oreos$milkabsgram[oreos$treatment == "DS"])

# Standard deviation for each cookie type
Regsd <- sd(oreos$milkabsgram[oreos$treatment == "Reg"])
TJsd <- sd(oreos$milkabsgram[oreos$treatment == "TJ"])
DSsd <- sd(oreos$milkabsgram[oreos$treatment == "DS"])

# part c
DSsize <- length(oreos$milkabsgram[oreos$treatment == "DS"])

regMean + qt(.975, df=(Regsize-1))*Regsd/sqrt(Regsize)
regMean - qt(.975, df=(Regsize-1))*Regsd/sqrt(Regsize)

TJmean + qt(.975, df=(TJsize-1))*TJsd/sqrt(TJsize)
TJmean - qt(.975, df=(TJsize-1))*TJsd/sqrt(TJsize)

DSMean + qt(.975, df=(DSsize-1))*DSsd/sqrt(DSsize)
DSMean - qt(.975, df=(DSsize-1))*DSsd/sqrt(DSsize)

# part d
boxplot(milkabsgram~treatment, data=oreos, ylab="Milk Absorbed in 10 Seconds (grams)", main = "Oreo Dunking Boxplots")

# part e
dotplot(milkabsgram~treatment, data=oreos, ylab="Milk Absorbed in 10 Seconds (grams)", main = "Oreo Dunking Dotplots")

# part h
anova(aov(milkabsgram~treatment, data=oreos))

# part j
TukeyHSD(aov(milkabsgram~treatment, data=oreos))

# part k
oreos$resids <- resid(aov(milkabsgram~treatment, data=oreos))
oreos$resids

# part l
plot(oreos$resids, type = "b", main = "Residuals vs. Order Collected", ylab = "Residuals")
abline(h = 0)

# part m
qqnorm(oreos$resids, main = "Normal QQ Plot of Residuals")
qqline(oreos$resids)

