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



#경찰서

경찰서 <- read_excel("data/safe/경찰청_경찰관서 위치 주소 현황_20230811.xls")

#경찰서 <- mutate(경찰서, 시군구 = sapply(주소, convert_to_district_or_city_and_last_word))

#경찰서 <- 경찰서 %>% mutate(시군구 = sapply(주소, convert_to_district_or_city_and_last_word))
#city_district



# NEW 시군구 변수 추가하는 함수

경찰서 <- 경찰서 %>% 
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

경찰서 <- 경찰서 %>% unite(시군구, 도, 시, 군, 구, sep = " ", na.rm = TRUE)


# 새로운 열에서 NA 값이 있는 경우 다음 열의 값을 가져와 채움

#부속의료기관 <- 부속의료기관 %>% unite(시군구, 도, 시, 군, 구, sep = " ", na.rm = TRUE)

#부속의료기관$시군구 <- gsub("^\\s+", "", 부속의료기관$시군구)


code <- read_excel("codecode.xlsx")

code <- distinct(code)
code$시도 <- sub("강원도", "강원특별자치도", code$시도)

#code <- code %>% mutate(code = paste(시도,시군구, sep = ""))
code <- code %>% mutate(시군구 = paste(시도,시군구, sep = " "))
code <- rename(code, code = 법정동코드)
code <- code %>% mutate(시군구 = ifelse(시군구 == "세종특별자치시 NA" , "세종특별자치시",시군구))

경찰서 <- left_join(경찰서, code, by = "시군구" )

경찰서 <- 경찰서 %>%
  filter(구분 != "폐지") %>%
  group_by(code) %>%
  summarise(경찰서 = n())
