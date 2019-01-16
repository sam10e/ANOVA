food <- read.table(text = "balance,color,attractive
1,1,99.83
1,2,99.37
2,2,92.9
2,1,89.58
2,1,87.82
2,1,79.31
1,1,79.16
1,2,78.48
1,2,72.14
1,2,69.92
1,2,68.86
2,1,68.78
2,2,66.77
1,1,62.95
1,2,62.3
2,2,60.78
1,2,54.07
2,1,49.46
1,1,47.78
2,2,46.41
1,1,44.48
1,1,44.13
1,2,42.94
2,2,42.63
1,2,42
1,1,40.3
1,2,38.95
1,2,35.03
1,2,34.99
1,2,32.71
2,1,32.5
2,2,32.46
1,1,31.71
1,2,29.62
2,1,29.02
2,1,28.89
1,2,25.98
1,2,22.93
1,1,17.68
1,2,17.6
2,1,13.78
2,2,13.17
2,2,13.15
2,1,11.84
2,2,7.94
2,2,7.7
2,2,3.43
2,2,3.34
2,1,1.64
2,1,-1.11
1,1,-4.51
1,1,-5.1
2,1,-6.61
1,1,-14.51
1,1,-15.06
2,2,-15.74
1,1,-20.3
2,1,-20.88
2,2,-22.9
1,1,-23.21
2,1,-25.82
1,1,-32.73
2,2,-40.28
2,2,-43.01
2,1,-47.69
2,2,-57.96
2,1,-60.7
1,1,-66.99", header = TRUE, sep=",")

str(food)
head(food)
tail(food)

food$balance <- as.factor(food$balance)
food$color <- as.factor(food$color)

foodMod <- aov(attractive~balance+color+balance:color, data = food)
anova(foodMod)

with(food, interaction.plot(response = attractive, 
                                x.factor = balance, 
                                trace.factor = color, 
                                ylab="Attractiveness", 
                                xlab = "balance", 
                                trace.lab = "color", 
                                main="Interaction Plot"))

TukeyHSD(foodMod, which="balance:color")

food$resids <- resid(foodMod)

qqnorm(food$resids)
qqline(food$resids)

plot(1:68, food$attractive, type="b")
