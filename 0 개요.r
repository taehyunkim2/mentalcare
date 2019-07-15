#
load("mentalcare.RData")
save.image("mentalcare.RData")

# 투수코치, 감독이 올라왔을 때 실점하지않고 자신이 맡은 이닝을 잘 마무리하는지를 보고싶다.

# 목차

# 데이터는 어떻게 얻었나
# 분석툴은 R
# 척도는 실점, 스케일은 정규이닝까지
# 에러를 더이상 파악하기 힘듦, 연장에서는 다른 마음과짐과 수싸움이 많음. -> 9이닝까지로 좁히자

## EDA
# 총 4432번 올라와서 격려했다.
length(trimmed_climbing[, 1])
# 이닝 히스토그램 -> 특정한 의미 없다.
hist(trimmed_climbing[, "inning"])
# 마진 히스토그램 -> 접전일 경우 훨씬 많이 올라온다.
hist(trimmed_climbing[, "margin"])
# 코치가 몇번, 매니저가 몇번
sum(trimmed_climbing[, "C/M"] == "manager")
sum(trimmed_climbing[, "C/M"] == "coach")
# 팀별로 봤을때는 어떤 팀이 가장 많이 올라왔고
a <- data.frame(summary(as.factor(trimmed_climbing[, "team"])), stringsAsFactors = F)
a2 <- data.frame(teams)
writexl::write_xlsx(a, path = "a.xlsx")
writexl::write_xlsx(a2, path = "a2.xlsx")
# 선발, 불펜 기준으로 어디가 더 많이 올라왔고
sum(trimmed_climbing[, 13] == "starter")
sum(trimmed_climbing[, 13] == "bullpen")

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
# 투코, 감독이 올라온 후 실점률이 올라갔다 -> 투수의 상태가 좋지 않을 때 올라온다는 의미.
# 올라왔을때 성공률, 올라왔을때 변경되는 실점률과 승률의 관계는 양의 상관관계가 있다.
# 선발투수가 멘탈이 흔들린 경우 빠른 교체,
# 불펜투수의 멘탈을 잡을 수 있다면, 승리를 부를 수 있는 도박수.