# data
load("mentalcare.RData")
save.image("mentalcare.RData")

# Anaysis that whether pitchers can finish their pitching after the coach or manager climbed on mound

## EDA
# climed 4,432 times total
length(trimmed_climbing[, 1])
# inning -> no pattern
ggplot(trimmed_climbing, aes(x=inning)) + geom_histogram()
# margin -> the absolute value of margin gets higher, they climbed more
ggplot(trimmed_climbing, aes(x=margin)) + geom_histogram()
# coach vs. manager
sum(trimmed_climbing[, "C/M"] == "coach")
sum(trimmed_climbing[, "C/M"] == "manager")
# team
ggplot(trimmed_climbing, aes(x=team)) + geom_histogram(stat="count")
# starter vs. reliever
sum(trimmed_climbing[, "S/R"] == "starter")
sum(trimmed_climbing[, "S/R"] == "bullpen")
# starter vs. reliever by team
plot(as.factor(teams), climbed_team[, "total_starter"]/climbed_team[,"total_bullpen"])

## analysis
# 시즌방어율 - 올라온 후 방어율 차이 -> 흔들린다는 증거.
# 선발은 실점률, 불펜은 성공률로 보자
writexl::write_xlsx(climbed_team, path = "climbed_team.xlsx")
## 선발
# 누가 제일 많이 올라왔는지
summary(as.factor(trimmed_climbing[which(trimmed_climbing[, 13] == "starter"), "team_pitcher"]))
# 위 투수들의 실점률 상승분은? 개인별 성적과 비교
summary(as.factor(trimmed_climbing[which(trimmed_climbing[, 13] == "starter"), "team_pitcher"]))[1:5]
# 팀별로 늘어나는거 보여줌
plot(climbed_team[, "r/o_starter"], climbed_team[, "r/ip_starter_season"])
plot(climbed_team[, "r/o/r/ip_starter_season"], climbed_team[, "w/l_season"])
# 팀별로 투수 수준차이가 있기 때문에 가중치선발평균과 비교
cor.test(climbed_team[, "r/o/r/ip_starter_season"], climbed_team[, "w/l_season"], alternative = "less")
## 불펜
# 누가 제일 많이 올라왔는지
summary(as.factor(trimmed_climbing[which(trimmed_climbing[, 13] == "bullpen"), "team_pitcher"]))
# 위 투수들의 성공률은? 전체평균과 비교
summary(as.factor(trimmed_climbing[which(trimmed_climbing[, 13] == "bullpen"), "team_pitcher"]))[1:5]
# 팀별 성공률
plot(climbed_team[, "p/t_bullpen"], climbed_team[, "w/l_season"])
# 투수운용의 성공률 - 팀승률과 비교
cor.test(climbed_team[, 11], climbed_team[, "p/t_bullpen"], alternative = "greater")

# 결론
# 투수코치, 감독이 올라온 후 실점률이 올라갔다 -> 투수의 상태가 좋지 않을 때 올라온다는 의미.
# 올라왔을때 성공률, 올라왔을때 변경되는 실점률과 승률의 관계는 양의 상관관계가 있다.
# 선발투수가 멘탈이 흔들린 경우 빠른 교체,
# 불펜투수의 멘탈을 잡을 수 있다면, 승리를 부를 수 있는 도박수.