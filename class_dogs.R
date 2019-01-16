# Notes 11/29/2018
# Objective: Measure rate of turnover of lactic acid in Cori cycle
# Compare 2 methods: injection of radioactive carbon14 traur or continuous infusion of radioactive carbon14
# Subjects: Dogs -> Randomly assigned to control or diabetes (surgery to remove pancreas)
# 2 measurements on each dog: Injection or Continous infusion (randomize order)


# Read in data
dogs <- read.table(text = "dog,operation,method,rate
0,control,inject,44
0,control,infuse,28
4,control,inject,33
4,control,infuse,23
5,control,inject,38
5,control,infuse,34
21,control,inject,59
21,control,infuse,19
23,control,inject,46
23,control,infuse,26
16,diabetic,inject,54
16,diabetic,infuse,42
17,diabetic,inject,43
17,diabetic,infuse,23
18,diabetic,inject,55
18,diabetic,infuse,23
19,diabetic,inject,71
19,diabetic,infuse,27
24,diabetic,inject,57
24,diabetic,infuse,35", header = TRUE, sep = ",")

# Look at data
str(dogs)
head(dogs)
tail(dogs)

# change dog to a factor
dogs$dog <- as.factor(dogs$dog)
str(dogs)

# Calculate the summary statistics
aggregate(rate~operation+method, data = dogs, FUN = mean)
aggregate(rate~operation+method, data = dogs, FUN = sd)
aggregate(rate~operation+method, data = dogs, FUN = length)

# Repeated measurement on same dog
library(lattice)
xyplot(rate~method|operation, groups = dog, type = "a", ylab = "Rate", xlab = "Method", data = dogs)

# Fit the model
dogmod <- aov(rate~operation+method+operation*method+Error(dog), data = dogs)
summary(dogmod)

# Fit the Bad model
dogsBADmod <- aov(rate~operation+method+operation*method+dog, data=dogs)
anova(dogsBADmod)
# F and p values differ from correct model because not separated into different tables to do the calculations

# Calculate pair wise
TukeyHSD(dogsBADmod, which="method")

