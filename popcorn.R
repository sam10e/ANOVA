# Homework 10
# Sam Tenney

# Read in the data
popcorn <- read.table(text = "Brand,Temp,Box,Bag,% Popped
Expensive,Room,1,1,84
Expensive,Frig,1,2,76
Expensive,Room,2,3,86
Expensive,Frig,2,4,86
Expensive,Room,3,5,91
Expensive,Frig,3,6,84
Generic,Room,4,7,74
Generic,Frig,4,8,87
Generic,Room,5,9,84
Generic,Frig,5,10,83
Generic,Room,6,11,83
Generic,Frig,6,12,90", header = TRUE, sep = ",")

# Look at the data
str(popcorn)
head(popcorn)
tail(popcorn)

# Repeated Measures plot
library(lattice)
xyplot(X..Popped~Temp|Brand, groups = Box, type = "a", ylab = "% Popped", xlab = "Temperature", main = "Part L: Repeated Measures Plot", data = popcorn)

# Fit ANOVA Model
popcornMod <- aov(X..Popped~Brand+Temp+Brand*Temp+Error(Box), data = popcorn)
summary(popcornMod)

# Create plot for main effects
xyplot(X..Popped~Brand,
       type = "a", 
       ylab = "% Popped", 
       xlab = "Brand", 
       main = "Part O: Main Effects Plot (Brand)",
       data = popcorn)

xyplot(X..Popped~Temp,
       type = "a", 
       ylab = "% Popped", 
       xlab = "Temp", 
       main = "Part O: Main Effects Plot (Temp)",
       data = popcorn)

# Create a plot for the interaction effect
interaction.plot(response = popcorn$X..Popped, 
                 x.factor=popcorn$Brand, 
                 trace.factor=popcorn$Temp, 
                 ylab="Percentage of Kernels Popped", 
                 xlab = "", 
                 trace.lab = "", 
                 main="Interaction Plot")

# Calculate grand mean, mean for each Brand, and the Brand effects
mean(popcorn$X..Popped)
brandMean <- aggregate(X..Popped~Brand, data=popcorn, FUN=mean)
brandMean

# Calculate the grand mean, the mean for each Temperature, and the Temperature effects
mean(popcorn$X..Popped)
tempMean <- aggregate(X..Popped~Temp, data=popcorn, FUN=mean)
tempMean

# Calculate the grand mean, the mean for each Brand x Temperature, and the interaction effects
mean(popcorn$X..Popped)
brandTempMean <- aggregate(X..Popped~Brand+Temp, data=popcorn, FUN=mean)
brandTempMean

