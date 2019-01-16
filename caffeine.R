# Sam Tenney
# Homework 8
# Caffeine.R

# Read in the data from https://blades.byu.edu/stat230data/caffeine.txt
caffeine <- read.table(text = "run sweetener carbonation taste
 1  CornSyrup No           189
 2  Aspertame No           187
 3  CornSyrup Yes          191
 4  AceK      Yes          173
 5  Aspertame Yes          171
 6  AceK      No           180
 7  Sugar     No           187
 8  CornSyrup Yes          184 
 9  Aspertame No           187
10  AceK      No           185
11  Sugar     No           190
12  AceK      Yes          163
13  Sugar     Yes          198
14  Sugar     Yes          199
15  CornSyrup No           182
16  Aspertame Yes          178", header = TRUE, sep = '')

str(caffeine)
head(caffeine)
tail(caffeine)

# Calculate the summary statistics for each treatment
aggregate(taste~sweetener+carbonation, data = caffeine, FUN = mean)
aggregate(taste~sweetener+carbonation, data = caffeine, FUN = sd)

# Create side-by-side dotplots of the response for the different treatments
library(lattice)
dotplot(taste~sweetener|carbonation, data = caffeine, main = "Dotplots of Taste Scores")

# Create ANOVA table 
caffeineFacMod <- aov(taste~sweetener+carbonation+sweetener:carbonation, data = caffeine)
anova(caffeineFacMod)

# Create table of 95% confidence intervals for all pair-wise comparisons of factor and interaction levels
TukeyHSD(caffeineFacMod, which="sweetener:carbonation")

# Create a graphic to show if there's a significant interaction
with(caffeine, interaction.plot(response = taste, 
                                x.factor = sweetener, 
                                trace.factor = carbonation, 
                                ylab="Mean Taste Scores", 
                                xlab = "", 
                                trace.lab = "", 
                                main="Interaction Plot"))


# Check Assumptions
# Calculate Residuals
caffeine$resids <- resid(caffeineFacMod)
mean(caffeine$resids)

# Index Plot: Check Independence
plot(caffeine$resids, type="b")
abline(h=0)

# Normal qq plot: check normality
qqnorm(caffeine$resids)
qqline(caffeine$resids)

# Ratio of sds: check equal variance
aggregate(taste~sweetener+carbonation, data = caffeine, FUN = sd)
