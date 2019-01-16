# Sam Tenney
# Hand Washing Technique

bacteria <- read.table(text = "count
700.67
420.82
278.78
647.36
361.25
164.19
154.06
356.10
274.30
319.89", header = TRUE, sep="")

bacteria_count_sd <- sd(bacteria$count)

d <- mean(bacteria$count) * 0.20

power.t.test(sig.level = .05, delta = d, power = .80, sd = bacteria_count_sd)
power.t.test(sig.level = .05, delta = d, power = .90, sd = bacteria_count_sd)

