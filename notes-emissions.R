# Notes from 9/25/2018
# Example of emissions data
# Response: Hydrocarbon emission(ppm)@ idlingspeed
# Explanatory: yr of manufacture
# (pre-1967, 1968-1974)

# Read in the data
emissions <- read.table(text = "
Year Emission
Pre-67 2351
Pre-67 1293
Pre-67 541
Pre-67 1058
Pre-67 411
Pre-67 570
Pre-67 800
Pre-67 630
Pre-67 905
Pre-67 347
Pre-67 620
Pre-67 940
Pre-67 350
Pre-67 700
Pre-67 1150
Pre-67 2000
Pre-67 823
Pre-67 1058
Pre-67 423
Pre-67 900
Pre-67 405
Pre-67 780
Pre-67 270
68-74 1088
68-74 388
68-74 111
68-74 558
68-74 294
68-74 211
68-74 460
68-74 470
68-74 353
68-74 71
68-74 241
68-74 2999
68-74 199
68-74 188
68-74 353
68-74 117
68-74 141
68-74 359
68-74 247
68-74 940
68-74 882
68-74 494
68-74 306
68-74 200
68-74 100
68-74 300
68-74 223
68-74 190
68-74 140
68-74 880
68-74 200
68-74 223
68-74 188
68-74 435
68-74 940
68-74 241
68-74 140
68-74 160
68-74 20
68-74 20
68-74 223
68-74 60
68-74 20
68-74 95
68-74 360
68-74 70
68-74 220
68-74 400
68-74 58
68-74 235
68-74 217
68-74 1880
68-74 200
68-74 175
68-74 85", sep="", header=TRUE)
# Check data
head(emissions)
tail(emissions)
str(emissions)

# Look at data
boxplot(Emission~Year, data=emissions)
library(lattice)
dotplot(Emission~Year, data=emissions)

# Calculate summary statistics
aggregate(Emission~Year, data=emissions, FUN=mean)
aggregate(Emission~Year, data=emissions, FUN=sd)
aggregate(Emission~Year, data=emissions, FUN=length)

# t-test
t.test(Emission~Year, data=emissions, var.equal=TRUE)

# Check assumptions
emissions$resids <-resid(aov(Emission~Year, data=emissions))

# Make qqplot
qqnorm(emissions$resids)
qqline(emissions$resids)

# transform the response
emissions$logEmission <- log(emissions$Emission)
boxplot(logEmission~Year, data=emissions)
dotplot(logEmission~Year, data=emissions)

# Calculate summary statistics
aggregate(logEmission~Year, data=emissions, FUN=mean)
aggregate(logEmission~Year, data=emissions, FUN=sd)
aggregate(logEmission~Year, data=emissions, FUN=length)

# t-test
t.test(logEmission~Year, data=emissions, var.equal=TRUE)

# Check assumptions
emissions$resids2 <-resid(aov(logEmission~Year, data=emissions))

# Make qqplot
qqnorm(emissions$resids2)
qqline(emissions$resids2)

# SImulate normal data
set.seed(40)
y <- rnorm(82)
qqnorm(y)
qqline(y)