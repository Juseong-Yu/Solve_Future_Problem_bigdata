# 라이브러리 불러오기
library(readxl)
library(dplyr)
library(stringr)
library(tidyverse)

# 데이터 불러오기
교원_학생수2019 <- read_excel("data/school/교원1인당_학생수_2019.xlsx")
교원_학생수2023 <- read_excel("data/school/교원1인당_학생수2023.xlsx")
code <- read_excel("codecode.xlsx")

# 데이터 정제하기
교원_학생수2019 <-교원_학생수2019  %>% unite(시군구, 시도,시군구, sep = " ", na.rm = TRUE)
교원_학생수2023 <-교원_학생수2023  %>% unite(시군구, 시도,시군구, sep = " ", na.rm = TRUE)

code <- distinct(code)
code$시도 <- sub("강원도", "강원특별자치도", code$시도)
#code <- code %>% mutate(code = paste(시도,시군구, sep = ""))
code <- code %>% mutate(시군구_tmp = paste(시도,시군구, sep = " "))
code <- rename(code, code = 법정동코드)
code <- code %>%
  mutate(시군구 = str_extract(시군구_tmp, "^[[:alnum:]]+\\s+[[:alnum:]]+"))
code <- code %>% mutate(시군구 = ifelse(시군구 == "세종특별자치시 NA" , "세종특별자치시",시군구))

교원_학생수2019 <- right_join(교원_학생수2019, code, by = "시군구" )
교원_학생수2023 <- right_join(교원_학생수2023, code, by = "시군구" )

# 데이터 분석을 위한 데이터만 남기기
교원_학생수2019 <- 교원_학생수2019 %>% select(code,교원1인당_학생수)
교원_학생수2023 <- 교원_학생수2023 %>% select(code,교원1인당_학생수)