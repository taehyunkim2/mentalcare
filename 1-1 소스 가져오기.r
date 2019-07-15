# install.packages("readxl")
# install.packages("stringr")
library(readxl) # read_excel
library(stringr) # 문자열 다루기

# source from : https://www.baseball-reference.com/leagues/MLB/2018-schedule.shtml
schedule_raw <- data.frame(read_excel("schedule_2018.xlsx", col_names = "data"), stringsAsFactors = F)

# >>로 시작되는 행 없애기
i <- 1
while(i <= length(schedule_raw[,1]))
{
  if(substr(schedule_raw[i,], 4, 4) == "»")
  {
    schedule_raw <- data.frame(schedule_raw[-i,], stringsAsFactors = F)
  } else
  {
    i <- i+1
  }
}

# 일정 행인경우 2열에 추가하기
i <- 1
while(i <= length(schedule_raw[,1]))
{
  if(sum(str_detect(schedule_raw[i,1],
                 c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))))
  {
    flag <- i
    schedule_raw[i, 2] <- "Day"
  } else
  {
    schedule_raw[i,2] <- as.character(schedule_raw[flag, 1])
  }
  i <- i+1
}
rm(flag)

# 일정 행 없애기
i <- 1
while(i <= length(schedule_raw[,1]))
{
  if(schedule_raw[i, 2] == "Day")
  {
    schedule_raw <- schedule_raw[-i, ]
  } else
  {
    i <- i+1
  }
}

# Date 형식으로 3열에 저장 후 한국시간 만들기
i <- 1
while(i <= length(schedule_raw[,1]))
{
  schedule_raw[i, 3] <- as.Date(schedule_raw[i,2], format = "%A, %B %d, %Y") + 1
  i <- i+1
}

# schedule 벡터 1열에 날짜 저장
schedule <- data.frame(NA)
i <- 1
while(i <= length(schedule_raw[,1]))
{
  schedule[i, 1] <- as.character(format(schedule_raw[i, 3], "%Y%m%d"))
  i <- i+1
}

# 팀 코드 벡터 만들기
teams <- c("Braves", "Nationals", "Phillies", "Mets", "Marlins",
           "Brewers", "Cubs", "Cardinals", "Pirates", "Reds",
           "Dodgers", "Rockies", "D'Backs", "Giants", "Padres",
           "Red Sox", "Yankees", "Rays", "Blue Jays", "Orioles",
           "Indians", "Twins", "Tigers", "White Sox", "Royals",
           "Astros", "Athletics", "Mariners", "Angels", "Rangers")
teamscode <- c("AT", "MO", "PH", "NM", "FL",
               "MI", "CC", "SL", "PI", "CI",
               "LA", "CO", "AZ", "SF", "SD",
               "BO", "NY", "TB", "TO", "BA",
               "CL", "MN", "DE", "CW", "KC",
               "HO", "OA", "SE", "AN", "TE")

# 팀명을 팀 코드로 만들어 원정, 홈으로 넣기
i <- 1
while(i <= length(schedule_raw[,1]))
{
  schedule[i, 2] <- teamscode[which(str_detect(substr(schedule_raw[i, 1], 1, str_locate(schedule_raw[i, 1], "@")[1]), teams))]
  schedule[i, 3] <- teamscode[which(str_detect(substr(schedule_raw[i, 1], str_locate(schedule_raw[i, 1], "@")[1], nchar(schedule_raw[i, 1])), teams))]
  i <- i+1
}

# 더블헤더 벡터, 더블헤더시 URL 맨 뒤 0이아닌 1, 2로 표시
schedule[, 4] <- 0
i <- 1
while(i <= (length(schedule[,1])-1))
{
  if(sum(paste0(schedule[i, 1], schedule[i, 2], schedule[i, 3])
         == paste0(schedule[(i+1), 1], schedule[(i+1), 2], schedule[(i+1), 3])))
  {
    schedule[i, 4] <- 1
    schedule[(i+1), 4] <- 2
    i <- i+2
  } else
  {
    i <- i+1
  }
}

# 20180913 FL @NM 이 경기는 더블헤더로 치뤄질 예정이었으나 2경기만 연기
# 예외적으로 1로 처리하도록 한다.
schedule[2180, 4] <- 1

# 최종 점수 벡터
i <- 1
while(i <= (length(schedule_raw[,1])-1))
{
  start <- str_locate_all(schedule_raw[i, 1], "\\(")[[1]][,1]
  end <- str_locate_all(schedule_raw[i, 1], "\\)")[[1]][,1]
  schedule[i, 5] <- as.numeric(substr(schedule_raw[i, 1], start[1]+1, end[1]-1))
  schedule[i, 6] <- as.numeric(substr(schedule_raw[i, 1], start[2]+1, end[2]-1))
  i <- i+1
}
rm(start, end, i)

# 게임코드 만들기
schedule[, 7] <- paste0(schedule[, 1], schedule[, 2], schedule[, 3], schedule[, 4])

# 최종 일정벡터 만들기
colnames(schedule) <- c("koreandate", "away", "home", "dh",
                        "score_away", "score_home", "game_code")