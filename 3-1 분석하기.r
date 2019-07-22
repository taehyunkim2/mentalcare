# install.packages("stringr")
library(stringr) # 문자열 다루기
# install.packages("writexl")
library(writexl) # 엑셀로 저장

# edited source
source_edited <- list(NA)
for(i in 1  :length(schedule[,1]))
{
  print(i)
  source_edited[[i]] <- gather_source(source_raw[i, 1])
  i <- i+1
}

# climbing data reference
climbing <- data.frame(matrix(rep(NA, 8), ncol=8), stringsAsFactors = F)
climbing_count <- 0
manager_climbing_for_change <- 0
coach_climbing_for_change <- 0

# climbing data count
for(game_number in 1:length(source_edited))
{
  # 게임 정보 변수 생성
  away_team_code <- schedule[game_number, 2]
  home_team_code <- schedule[game_number, 3]
  # 어웨이
  for(away_inning in 1:length(source_edited[[game_number]]$away))
  {
    for(i in 1:length(source_edited[[game_number]]$away[[away_inning]][,1]))
    {
      if(str_detect(source_edited[[game_number]]$away[[away_inning]][i, 1], "감독 마운드에 오름"))
      {
        if(source_edited[[game_number]]$away[[away_inning]][i, 2] == source_edited[[game_number]]$away[[away_inning]][(i+1), 2])
        {
          climbing_count <- climbing_count + 1
          climbing[climbing_count, 1] <- game_number
          climbing[climbing_count, 2] <- schedule[game_number, 1]
          climbing[climbing_count, 3] <- away_team_code
          climbing[climbing_count, 4] <- "away"
          climbing[climbing_count, 5] <- away_inning
          climbing[climbing_count, 6] <- source_edited[[game_number]]$away[[away_inning]][i, 2]
          climbing[climbing_count, 7] <- source_edited[[game_number]]$away[[away_inning]][i, 5]
          climbing[climbing_count, 8] <- i
          climbing[climbing_count, 9] <- "manager"
        } else
        {
          manager_climbing_for_change <- manager_climbing_for_change + 1
        }
      } else if(str_detect(source_edited[[game_number]]$away[[away_inning]][i, 1], "투수코치 마운드에 오름"))
      {
        if(source_edited[[game_number]]$away[[away_inning]][i, 2] == source_edited[[game_number]]$away[[away_inning]][(i+1), 2])
        {
          climbing_count <- climbing_count + 1
          climbing[climbing_count, 1] <- game_number
          climbing[climbing_count, 2] <- schedule[game_number, 1]
          climbing[climbing_count, 3] <- away_team_code
          climbing[climbing_count, 4] <- "away"
          climbing[climbing_count, 5] <- away_inning
          climbing[climbing_count, 6] <- source_edited[[game_number]]$away[[away_inning]][i, 2]
          climbing[climbing_count, 7] <- source_edited[[game_number]]$away[[away_inning]][i, 5]
          climbing[climbing_count, 8] <- i
          climbing[climbing_count, 9] <- "coach"
        } else
        {
          coach_climbing_for_change <- coach_climbing_for_change + 1
        }
      }
    }
  }
  # 홈
  for(home_inning in 1:length(source_edited[[game_number]]$home))
  {
    for(i in 1:length(source_edited[[game_number]]$home[[home_inning]][,1]))
    {
      if(str_detect(source_edited[[game_number]]$home[[home_inning]][i, 1], "감독 마운드에 오름"))
      {
        if(source_edited[[game_number]]$home[[home_inning]][i, 2] == source_edited[[game_number]]$home[[home_inning]][(i+1), 2])
        {
          climbing_count <- climbing_count + 1
          climbing[climbing_count, 1] <- game_number
          climbing[climbing_count, 2] <- schedule[game_number, 1]
          climbing[climbing_count, 3] <- home_team_code
          climbing[climbing_count, 4] <- "home"
          climbing[climbing_count, 5] <- home_inning
          climbing[climbing_count, 6] <- source_edited[[game_number]]$home[[home_inning]][i, 2]
          climbing[climbing_count, 7] <- source_edited[[game_number]]$home[[home_inning]][i, 5]
          climbing[climbing_count, 8] <- i
          climbing[climbing_count, 9] <- "manager"
        } else
        {
          manager_climbing_for_change <- manager_climbing_for_change + 1
        }
      } else if(str_detect(source_edited[[game_number]]$home[[home_inning]][i, 1], "투수코치 마운드에 오름"))
      {
        if(source_edited[[game_number]]$home[[home_inning]][i, 2] == source_edited[[game_number]]$home[[home_inning]][(i+1), 2])
        {
          climbing_count <- climbing_count + 1
          climbing[climbing_count, 1] <- game_number
          climbing[climbing_count, 2] <- schedule[game_number, 1]
          climbing[climbing_count, 3] <- home_team_code
          climbing[climbing_count, 4] <- "home"
          climbing[climbing_count, 5] <- home_inning
          climbing[climbing_count, 6] <- source_edited[[game_number]]$home[[home_inning]][i, 2]
          climbing[climbing_count, 7] <- source_edited[[game_number]]$home[[home_inning]][i, 5]
          climbing[climbing_count, 8] <- i
          climbing[climbing_count, 9] <- "coach"
        } else
        {
          coach_climbing_for_change <- coach_climbing_for_change + 1
        }
      }
    }
  }
  print(game_number)
}
climbing[, 10] <- as.character(paste0(climbing[, 3], "_", climbing[, 6]))
names(climbing) <- c("game_number", "koreandate", "team", "H/A", "inning", "pitcher", "margin", "index", "C/M", "team_pitcher")
rm(game_number, away_team_code, home_team_code, away_inning, home_inning, i)
summary(climbing)

# 연장에서의 에러를 잡지못해 신뢰도 하락 -> 연장에서 올라온 정보를 잘라냄
trimmed_climbing <- climbing[which(as.numeric(climbing[, 5]) <= 9), ]
trimmed_climbing[, 10] <- as.factor(trimmed_climbing[, 10])
summary(trimmed_climbing)

for(i in 1:(length(trimmed_climbing[, 1])-2))
{
  if((trimmed_climbing[i, 10] == trimmed_climbing[(i+2), 10]))
  {
    print(i)
  }
}

# 몇실점 했는지, 아웃카운트는 몇 개 잡았는지
climb_num <- 1
while(climb_num <= length(trimmed_climbing[, 1]))
{
  # 투수코치가 한번 더 올라왔거나 투수가 바뀔때까지
  if(trimmed_climbing[climb_num, "H/A"] == "away")
  {
    start_score <- source_edited[[trimmed_climbing[climb_num, "game_number"]]][[trimmed_climbing[[climb_num, "H/A"]]]][[trimmed_climbing[climb_num, "inning"]]][trimmed_climbing[climb_num, "index"], 4]
  } else
  {
    start_score <- source_edited[[trimmed_climbing[climb_num, "game_number"]]][[trimmed_climbing[[climb_num, "H/A"]]]][[trimmed_climbing[climb_num, "inning"]]][trimmed_climbing[climb_num, "index"], 3]
  }
  out_count <- 0
  dummy <- NA
  for(inning in trimmed_climbing[climb_num, "inning"]:length(source_edited[[trimmed_climbing[climb_num, "game_number"]]][[trimmed_climbing[climb_num, "H/A"]]]))
  {
    dummy <- rbind(dummy, source_edited[[trimmed_climbing[climb_num, "game_number"]]][[trimmed_climbing[climb_num, "H/A"]]][[inning]])
  }
  dummy <- dummy[-1, ]
  index <- trimmed_climbing[climb_num, "index"]
  while(index <= length(dummy[, 1]))
  {
    if((dummy[index, 2] != dummy[(index+1), 2]) || (index == length(dummy[, 1])))
    {
      break
    }
    if(str_detect(dummy[index, 1], "아웃"))
    {
      out_count <- out_count + 1
    }
    index <- index+1
  }
  if(trimmed_climbing[climb_num, "H/A"] == "away")
  {
    trimmed_climbing[climb_num, 11] <- dummy[index, 4] - start_score
  } else
  {
    trimmed_climbing[climb_num, 11] <- dummy[index, 3] - start_score
  }
  trimmed_climbing[climb_num, 12] <- out_count
  while( (trimmed_climbing[climb_num, 10] == trimmed_climbing[ (climb_num + 1), 10]) && (climb_num < length(trimmed_climbing[, 1])) )
  {
    climb_num <- climb_num + 1
    trimmed_climbing[climb_num, 11] <- 0
    trimmed_climbing[climb_num, 12] <- 0
  }
  print(climb_num)
  climb_num <- climb_num + 1
}
rm(start_score, climb_num, index, inning, out_count)
names(trimmed_climbing) <- c("game_number", "koreandate", "team", "H/A", "inning", "pitcher", "margin", "index", "C/M", "team_pitcher", "runaway", "out_count")

# 선발, 불펜 여부 추가
for(climb_num in 1:length(trimmed_climbing[, 1]))
{
  if(trimmed_climbing[climb_num, "H/A"] == "away")
  {
    if(pitcher_list(source_raw[trimmed_climbing[climb_num, "game_number"], 1])[[1]][1, 1] == trimmed_climbing[climb_num, "pitcher"])
    {
      trimmed_climbing[climb_num, 13] <- "starter"
    } else
    {
      trimmed_climbing[climb_num, 13] <- "bullpen"
    }
  } else
  {
    if(pitcher_list(source_raw[trimmed_climbing[climb_num, "game_number"], 1])[[2]][1, 1] == trimmed_climbing[climb_num, "pitcher"])
    {
      trimmed_climbing[climb_num, 13] <- "starter"
    } else
    {
      trimmed_climbing[climb_num, 13] <- "bullpen"
    }
  }
  print(climb_num)
}

# 실점이 없으면 성공, 있으면 실패
trimmed_climbing[, 14] <- trimmed_climbing[, 11]/(trimmed_climbing[, 12]/3)*9
for(climb_num in 1:length(trimmed_climbing[, 1]))
{
  if(trimmed_climbing[climb_num, 11] == 0 && trimmed_climbing[climb_num, 12] > 0)
  {
    trimmed_climbing[climb_num, 15] <- 1
  } else if(trimmed_climbing[climb_num, 11] != 0)
  {
    trimmed_climbing[climb_num, 15] <- 0
  } else
  {
    trimmed_climbing[climb_num, 15] <- NA
  }
}
rm(climb_num)
names(trimmed_climbing) <- c("game_number", "koreandate", "team", "H/A", "inning", "pitcher", "margin", "index", "C/M", "team_pitcher", "runaway", "out_count", "S/R", "today_era", "P/F")

writexl::write_xlsx(trimmed_climbing, path = "trimmed_climbing.xlsx")