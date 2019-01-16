# Sam Tenney
# stroopGroupX.R
# Homework 9

# Read in the data
stroop <- read.table(text = "Run Block blockRun Task timerDisplay Time
1	Thomas  1	Color	No	63.84
2	Thomas	2	Shape	No	69.069
3	Thomas	3	Shape	Yes	69.869
4	Thomas	4	Color	Yes	59.477
5	Sam	1	Shape	No	54.02
6	Sam	2	Color	Yes	51.949
7	Sam	3	Color	No	48.16
8	Sam	4	Shape	Yes	50.113
9	Wade	1	Shape	No	61
10 Wade	2	Color	No	60.997
11 Wade	3	Shape	Yes	57.159
12 Wade	4	Color	Yes	57.176
13 Cory	1	Color	No	59.205
14 Cory	2	Shape Yes	55.484
15 Cory	3	Shape	No	55.556
16 Cory	4	Color	Yes	52.77
", header = TRUE, sep = "")

# Look at the data
str(stroop)
head(stroop)
tail(stroop)

# Calculate summary statistics
aggregate(Time~Task+timerDisplay+Task*timerDisplay, data = stroop, FUN = mean)
aggregate(Time~Task+timerDisplay+Task*timerDisplay, data = stroop, FUN = sd)

# Display the data
library(lattice)
bwplot(Time~Task+timerDisplay, data = stroop)
dotplot(Time~Task+timerDisplay, data = stroop, jitter.x = TRUE, main = "Shapes and Color Times")

# Make an ANOVA table
stroopMod <- aov(Time~Task+timerDisplay+Task*timerDisplay + Block, data = stroop)
anova(stroopMod)

# Make ANOVA table without blocks
stroopMod2 <- aov(Time~Task+timerDisplay+Task*timerDisplay, data = stroop)
anova(stroopMod2)

# Check Assumptions
#Independent (index plot)
plot(stroop$Run, stroop$Time, type="b")

# Check mean of residuals to see if it equals 0
stroop$resids <- resid(stroopMod)
mean(stroop$resids)

# Normally distributed (qqplot)
qqnorm(stroop$resids)
qqline(stroop$resids)
# Constant variance (sd ratio)
aggregate(Time~Task+timerDisplay, data = stroop, FUN = sd)
