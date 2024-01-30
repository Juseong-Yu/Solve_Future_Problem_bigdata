#패키지
install.packages("stringr")
install.packages("readr")
install.packages("readxl")
install.packages("tidyr")
update.packages("dplyr")
library(dplyr)
library(stringr)
library(readr)
library(readxl)
library(tidyr)




#소방서

소방서 <- read_excel("data/safe/소방청_시도 소방서 현황_20200121.xls")

#소방서 <- mutate(소방서, 시군구 = sapply(주소, convert_to_district_or_city_and_last_word))



# NEW 시군구 변수 추가하는 함수

소방서 <- 소방서 %>% 
  mutate(
    도 = str_extract(주소, "([가-힣]+도)(?![가-힣0-9\\(])"),
    시 = str_extract(주소, "([가-힣]+시)(?![가-힣0-9\\(])"),
    군 = str_extract(주소, "([가-힣]+군)(?![가-힣0-9\\(])"),
    구 = str_extract(주소, "([가-힣]+구)(?![가-힣0-9\\(])")
  ) %>%
  mutate(
    구 = ifelse(시 == "대구광역시", str_extract(주소, "(?<=대구광역시[[:space:]])([가-힣]+구)"), 구)
  )

# 새로운 열에서 NA 값이 있는 경우 다음 열의 값을 가져와 채움

소방서 <- 소방서 %>% unite(시군구, 도, 시, 군, 구, sep = " ", na.rm = TRUE)


code <- read_excel("codecode.xlsx")

code <- distinct(code)
code$시도 <- sub("강원도", "강원특별자치도", code$시도)

#code <- code %>% mutate(code = paste(시도,시군구, sep = ""))
code <- code %>% mutate(시군구 = paste(시도,시군구, sep = " "))
code <- rename(code, code = 법정동코드)
code <- code %>% mutate(시군구 = ifelse(시군구 == "세종특별자치시 NA" , "세종특별자치시",시군구))


소방서 <- left_join(소방서, code, by = "시군구" )

소방서 <- 소방서 %>%
  group_by(code) %>%
  summarise(소방서 = n())


