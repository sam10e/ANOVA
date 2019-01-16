# Final Project
# Sam Tenney, Cory Staton, Jacob West

# Randomize the experiment
set.seed(1234)
sample(1:20, replace=FALSE)

# Read in the data
results <- read.table(text = "run,material,clipSize,time
2,Notebook,Small,2.10
3,Notebook,Small,1.98
4,Notebook,Big,1.86
5,Notebook,Small,2.24
6,Copier,Big,1.79
7,Copier,Small,1.64
8,Copier,Small,1.65
9,Copier,Big,1.72
10,Copier,Big,1.71
11,Copier,Big,1.64
12,Copier,Small,1.98
13,Notebook,Big,1.84
14,Notebook,Small,2.11
15,Copier,Small,1.73
16,Copier,Big,1.78
17,Notebook,Big,1.65
18,Notebook,Big,1.97
19,Notebook,Big,1.66
20,Notebook,Small,1.58", header = TRUE, sep = ",")

# Look at the data
str(results)
head(results)
tail(results)

# Summarize the results
aggregate(time~material+clipSize, data = results, FUN = mean)
aggregate(time~material+clipSize, data = results, FUN = sd)
aggregate(time~material+clipSize, data = results, FUN = length)

# Make ANOVA Table (Type III)
library(car)
options(contrasts = c("contr.sum", "contr.poly"))
resultsMod <- aov(time~material+clipSize+material*clipSize, data = results)
Anova(resultsMod, type = "III", contrasts = list(topic=contr.sum, sys=contr.sum))

# Calculate the grand mean
mean(results$time)
sd(results$time)

# Check Assumptions
# Calculate Residuals
results$resids <- resid(resultsMod)
mean(results$resids)

# Index Plot: Check Independence
plot(results$resids, type="b", main = "Index Plot", ylab = "Residuals")
abline(h=0)

# Normal qq plot: check normality
qqnorm(results$resids)
qqline(results$resids)

# Constant variance
0.253 / 0.061



