# install.packages("stringr")
library(stringr) # 문자열 다루기

# 주어진 character를 pattern부터 " 전까지로 끊어주는 함수
split_by_pattern <- function(game_source, pattern)
{
  relay_text <- data.frame(NA)
  locate_data <- str_locate_all(game_source, pattern)
  for(i in 1:length(locate_data[[1]][,2]))
  {
    for(j in (locate_data[[1]][i, 2]+1):nchar(game_source))
    {
      if(substr(game_source, j, j) == "\"")
      {
        flag <- j-1
        break
      }
    }
    relay_text[i, 1] <- substr(game_source, (locate_data[[1]][i,2]+1), (j-1))
  }
  return(relay_text)
}

# 원정, 홈의 투수 목록을 만들어주는 함수
pitcher_list <- function(game_source)
{
  pattern1 <- "pitcher\":\\["
  pattern2 <- "homeTeamLineUp"
  pattern3 <- "\"name\":\""
  locate1 <- str_locate_all(game_source, pattern1)[[1]][,1]
  locate2 <- str_locate_all(game_source, pattern2)[[1]][,1]
  nchar(game_source)
  away_pitcher_raw <- substr(game_source, locate1[1], as.numeric(locate2))
  home_pitcher_raw <- substr(game_source, locate1[2], nchar(game_source))
  away_pitcher <- split_by_pattern(away_pitcher_raw, pattern3)
  colnames(away_pitcher) <- "away_pitcher"
  home_pitcher <- split_by_pattern(home_pitcher_raw, pattern3)
  colnames(home_pitcher) <- "home_pitcher"
  return(list(away_pitcher, home_pitcher))
}

# 한 경기를 1~9회로 나눠 리스트에 저장하는 함수
split_by_inning <- function(game_source)
{
  inning_source <- list(NA)
  inning <- 1
  while(1)
  {
    pattern <- paste0("\"inn\":", inning, ",\"liveText\":\"")
    if(str_detect(game_source, pattern))
    {
      inning_source[[inning]] <- split_by_pattern(game_source, pattern)
      colnames(inning_source[[inning]]) <- paste0("inning ", inning)
      inning <- inning + 1
    } else
    {
      inning <- inning - 1
      break
    }
  }
  # "---"부분 없애기
  for(i in 1:length(inning_source[[inning]][, 1]))
  {
    if(str_detect(inning_source[[inning]][i, 1], "------------------------"))
    {
      inning_source[[inning]] <- data.frame(inning_source[[inning]][(1:(i-1)), 1], stringsAsFactors = F)
      if(str_detect(inning_source[[inning]][length(inning_source[[inning]][, 1]), 1], "타자"))
      {
        inning_source[[inning]] <- data.frame(c(inning_source[[inning]][1, 1],
                                                inning_source[[inning]][length(inning_source[[inning]][, 1]), 1],
                                                inning_source[[inning]][(2:(length(inning_source[[inning]][, 1])-1)), 1]),
                                              stringsAsFactors = F)
      }
      colnames(inning_source[[inning]]) <- paste0("inning ", inning)
      break
    }
  }
  return(inning_source)
}

# 각자 초(T), 말(B)로 쪼개고 뒤집기
split_by_TB <- function(game_source)
{
  inning_source <- split_by_inning(game_source)
  TB_source <- list(NA)
  iter <- 0
  for(inning in 1:length(inning_source))
  {
    posession <- 0
    posession_index <- NA
    for(i in 1:length(inning_source[[inning]][,1]))
    {
      if(str_detect(inning_source[[inning]][i, 1], "공격"))
      {
        posession <- posession + 1
        posession_index <- i
      }
    }
    if(posession == 2)
    {
      iter <- iter + 1
      TB_source[[iter]] <- data.frame(inning_source[[inning]][posession_index:length(inning_source[[inning]][, 1]), 1], stringsAsFactors = F)
      TB_source[[iter]] <- data.frame(TB_source[[iter]][seq(dim(TB_source[[iter]])[1],1),], stringsAsFactors = F)
      iter <- iter + 1
      TB_source[[iter]] <- data.frame(inning_source[[inning]][1:(posession_index-1), 1], stringsAsFactors = F)
      TB_source[[iter]] <- data.frame(TB_source[[iter]][seq(dim(TB_source[[iter]])[1],1),], stringsAsFactors = F)
    } else if(posession == 1)
    {
      iter <- iter+1
      TB_source[[iter]] <- data.frame(inning_source[[inning]][1:length(inning_source[[inning]][, 1]), 1], stringsAsFactors = F)
      TB_source[[iter]] <- data.frame(TB_source[[iter]][seq(dim(TB_source[[iter]])[1],1),], stringsAsFactors = F)
    } else
    {
      TB_source <- NA
      break
    }
  }
  return(TB_source)
}

# TB에 투수 정보(col2)와 현재 스코어(col3 away, col4 home), 마진(col5)을 입력
pitcher_and_current_score_data <- function(game_source)
{
  TB_source <- split_by_TB(game_source)
  pitcher <- pitcher_list(game_source)
  current_score_away <- 0
  current_score_home <- 0
  current_pitcher_away_index <- 1
  current_pitcher_home_index <- 1
  scored <- c("홈인", "홈런")
  for(TB in 1:length(TB_source))
  {
    if(TB %% 2 == 1)
    {
      for(i in 1:length(TB_source[[TB]][,1]))
      {
        if(str_detect(TB_source[[TB]][i, 1], paste0(pitcher[[2]][(current_pitcher_home_index + 1), 1]))) # 투수가 바뀌면
        {
          current_pitcher_home_index <- current_pitcher_home_index + 1
        }
        if(sum(str_detect(TB_source[[TB]][i, 1], scored))) # 점수가 나면
        {
          current_score_away <- current_score_away + 1
        }
        TB_source[[TB]][i, 2] <- pitcher[[2]][current_pitcher_home_index, 1]
        TB_source[[TB]][i, 3] <- current_score_away
        TB_source[[TB]][i, 4] <- current_score_home
        TB_source[[TB]][i, 5] <- current_score_home - current_score_away
      }
    } else
    {
      for(i in 1:length(TB_source[[TB]][,1]))
      {
        if(str_detect(TB_source[[TB]][i, 1], paste0(pitcher[[1]][(current_pitcher_away_index+1), 1])))
        {
          current_pitcher_away_index <- current_pitcher_away_index + 1
        }
        if(sum(str_detect(TB_source[[TB]][i, 1], scored))) # 점수가 나면
        {
          current_score_home <- current_score_home + 1
        }
        TB_source[[TB]][i, 2] <- pitcher[[1]][current_pitcher_away_index, 1]
        TB_source[[TB]][i, 3] <- current_score_away
        TB_source[[TB]][i, 4] <- current_score_home
        TB_source[[TB]][i, 5] <- current_score_away - current_score_home
      }
    }
    colnames(TB_source[[TB]]) <- c("relaytext", "pitcher", "away_score", "home_score", "margin")
  }
  return(TB_source)
}

# 결과 가져오기
gather_source <- function(game_source)
{
  TB_source <- pitcher_and_current_score_data(game_source)
  T_source <- list(NA)
  B_source <- list(NA)
  count_t <- 0
  count_b <- 0
  for(i in 1:length(TB_source))
  {
    if(i %% 2 == 1)
    {
      count_t <- count_t + 1
      T_source[[count_t]] <- TB_source[[i]]
    } else
    {
      count_b <- count_b + 1
      B_source[[count_b]] <- TB_source[[i]]
    }
  }
  names(T_source) <- paste0("T", 1:count_t)
  names(B_source) <- paste0("B", 1:count_b)
  source <- list(B_source, T_source)
  names(source) <- c("away", "home")
  return(source)
}