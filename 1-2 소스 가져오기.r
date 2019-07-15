# install.packages("rvest")
library(rvest) # read_html

# 크롤링 함수 만들기
source_crawling <- function(game_code)
{
  url <- paste0("https://sports.news.naver.com/ajax/game/relayData.nhn?gameId=", game_code)
  return(as.character(read_html(url)))
}

# 소스를 저장할 벡터를 0으로 초기화 후 크롤링
source_raw <- data.frame(rep("0", length(schedule[,1])), stringsAsFactors = F)
i <- 1
while(i <= length(schedule[, 1]))
{
  source_raw[i, 1] <- source_crawling(schedule[i, 7])
  print(i)
  i <- i+1
}
rm(i)

# 크롤링이 잘 되었나 확인
if(sum(source_raw == "0") == 0)
{
  print("Well Done!")
  rm(schedule_crawling)
}