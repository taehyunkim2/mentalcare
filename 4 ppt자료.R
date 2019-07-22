# ppt 자료
library(ggplot2)

plot(climbed_team[, 11], climbed_team[, 25])
cor(climbed_team[, 11], climbed_team[, 25])
plot(trimmed_climbing[, "margin"], trimmed_climbing[, "inning"])
plot(climbed_team[, 8], climbed_team[, 9])
plot(climbed_team[, 5], climbed_team[, 11])
cor(climbed_team[, 5], climbed_team[, 11])
plot(climbed_team[, 10], climbed_team[, 11])
cor(climbed_team[, 10], climbed_team[, 11])
cor(climbed_team[, 14], climbed_team[, 11])
sum(trimmed_climbing[, 11])/(sum(trimmed_climbing[, 12])/3)*9
sum(trimmed_climbing[which(trimmed_climbing[, 13] == "starter"), 11])/(sum(trimmed_climbing[which(trimmed_climbing[, 13] == "starter"), 12])/3)*9
sum(trimmed_climbing[which(trimmed_climbing[, 13] == "bullpen"), 11])/(sum(trimmed_climbing[which(trimmed_climbing[, 13] == "bullpen"), 12])/3)*9
summary(trimmed_climbing)
hist(trimmed_climbing[,11])
sum(trimmed_climbing[which(trimmed_climbing[, 10] == "AZ_고들리"), 11])
sum(trimmed_climbing[which(trimmed_climbing[, 10] == "AZ_고들리"), 12])/3
