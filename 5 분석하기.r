climbed_pitcher <- data.frame(as.character(unique(trimmed_climbing[, "team_pitcher"])))
for(pitcher_num in 1:length(climbed_pitcher[, 1]))
{
  climbed_pitcher[pitcher_num, 2] <- sum(trimmed_climbing[which(trimmed_climbing[, "team_pitcher"] == climbed_pitcher[pitcher_num, 1]), 11])
  climbed_pitcher[pitcher_num, 3] <- sum(trimmed_climbing[which(trimmed_climbing[, "team_pitcher"] == climbed_pitcher[pitcher_num, 1]), 12])
  climbed_pitcher[pitcher_num, 4] <- climbed_pitcher[pitcher_num, 2] / (climbed_pitcher[pitcher_num, 3]/3) * 9
  climbed_pitcher[pitcher_num, 5] <- sum(na.omit(trimmed_climbing[which(trimmed_climbing[, "team_pitcher"] == climbed_pitcher[pitcher_num, 1]), 15]))
  climbed_pitcher[pitcher_num, 6] <- sum(trimmed_climbing[, "team_pitcher"] == climbed_pitcher[pitcher_num, 1])
  climbed_pitcher[pitcher_num, 7] <- climbed_pitcher[pitcher_num, 5] / climbed_pitcher[pitcher_num, 6]
}

# 팀별
climbed_team <- data.frame(teams, teamscode) # 1, 2열 팀명
for(team_num in 1:length(climbed_team[, 1]))
{
  climbed_team[team_num, 3] <- sum(na.omit(trimmed_climbing[which(trimmed_climbing[, "team"] == climbed_team[team_num, 2]), 15]))
  climbed_team[team_num, 4] <- sum(trimmed_climbing[, "team"] == climbed_team[team_num, 2])
  climbed_team[team_num, 5] <- climbed_team[team_num, 3] / climbed_team[team_num, 4]
  climbed_team[team_num, 6] <- sum(trimmed_climbing[which(trimmed_climbing[, "team"] == climbed_team[team_num, 2]), 11])
  climbed_team[team_num, 7] <- sum(trimmed_climbing[which(trimmed_climbing[, "team"] == climbed_team[team_num, 2]), 12])
  climbed_team[team_num, 8] <- climbed_team[team_num, 6] / (climbed_team[team_num, 7]/3) * 9
}
climbed_team[, 9] <- c(4.060568603,4.244813278,4.533628564,4.357622244,5.049237171,4.059548255,3.932660389,4.273933063,4.349372385,5.115197779,3.719512195,4.617450589,3.961722488,4.305660119,4.737817433,3.993279386,4.135018199,4.014916097,5.224672063,5.610062893,4.002470661,4.833344883,5.027015648,5.311064718,5.235335196,3.303092784,4.14004914,4.418588593,4.521606012,5.333333333)
climbed_team[, 10] <- climbed_team[, 8] / climbed_team[, 9]
climbed_team[, 11] <- c(0.556,0.506,0.494,0.475,0.391,0.589,0.583,0.543,0.509,0.414,0.564,0.558,0.506,0.451,0.407,0.667,0.617,0.556,0.451,0.29,0.562,0.481,0.395,0.383,0.358,0.636,0.599,0.549,0.494,0.414)
for(team_num in 1:length(climbed_team[, 1]))
{
  climbed_team[team_num, 12] <- sum(na.omit(trimmed_climbing[which(trimmed_climbing[, "team"] == climbed_team[team_num, 2] & trimmed_climbing[, 13] == "bullpen"), 15]))
  climbed_team[team_num, 13] <- sum(trimmed_climbing[, "team"] == climbed_team[team_num, 2] & trimmed_climbing[, 13] == "bullpen")
  climbed_team[team_num, 14] <- climbed_team[team_num, 12] / climbed_team[team_num, 13]
}
for(team_num in 1:length(climbed_team[, 1]))
{
  climbed_team[team_num, 15] <- sum(na.omit(trimmed_climbing[which(trimmed_climbing[, "team"] == climbed_team[team_num, 2] & trimmed_climbing[, 13] == "starter"), 15]))
  climbed_team[team_num, 16] <- sum(trimmed_climbing[, "team"] == climbed_team[team_num, 2] & trimmed_climbing[, 13] == "starter")
  climbed_team[team_num, 17] <- climbed_team[team_num, 15] / climbed_team[team_num, 16]
}
climbed_team[, 18] <- c(3.803380783,4.26889107,4.49948636,3.810305218,4.525862069,4.271546635,4.10472973,3.876518219,4.301657085,5.454545455,3.512636994,4.345493562,4.034954407,4.367557852,5.375912409,4.091378717,4.305620065,3.865384615,5.534501251,5.984157465,3.633709223,4.829531813,5.153809749,5.513913824,5.142278709,3.382891844,4.532216964,4.656043093,4.6838902,5.79787234)
for(team_num in 1:length(climbed_team[, 1]))
{
  climbed_team[team_num, 19] <- sum(trimmed_climbing[which(trimmed_climbing[, "team"] == climbed_team[team_num, 2] & trimmed_climbing[, 13] == "starter"), 11])
  climbed_team[team_num, 20] <- sum(trimmed_climbing[which(trimmed_climbing[, "team"] == climbed_team[team_num, 2] & trimmed_climbing[, 13] == "starter"), 12])
  climbed_team[team_num, 21] <- climbed_team[team_num, 19] / (climbed_team[team_num, 20]/3) * 9
}
for(team_num in 1:length(climbed_team[, 1]))
{
  climbed_team[team_num, 22] <- sum(trimmed_climbing[which(trimmed_climbing[, "team"] == climbed_team[team_num, 2] & trimmed_climbing[, 13] == "bullpen"), 11])
  climbed_team[team_num, 23] <- sum(trimmed_climbing[which(trimmed_climbing[, "team"] == climbed_team[team_num, 2] & trimmed_climbing[, 13] == "bullpen"), 12])
  climbed_team[team_num, 24] <- climbed_team[team_num, 22] / (climbed_team[team_num, 23]/3) * 9
}
climbed_team[, 25] <- climbed_team[, 21] / climbed_team[, 18]
colnames(climbed_team) <- c("team", "teamcode", "pass", "total", "p/t_ratio", "runaway", "outcount", "r/o_total", "r/ip_season", "r/o/r/ip_season", "w/l_season",
                            "pass_bullpen", "total_bullpen", "p/t_bullpen",  "pass_starter", "total_starter", "p/t_starter", "r/ip_starter_season",
                            "r_starter", "o_starter", "r/o_starter", "r_bullpen", "o_bullpen", "r/o_bullpen", "r/o/r/ip_starter_season")