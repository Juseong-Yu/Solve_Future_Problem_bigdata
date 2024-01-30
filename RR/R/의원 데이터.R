#패키지
install.packages("stringr")
install.packages("readr")
install.packages("readxl")
install.packages("tidyr")
library(dplyr)
library(stringr)
library(readr)
library(readxl)
library(tidyr)



#의원

의원 <- read_excel("data/hospital/전국 의원 데이터.xls")

# NEW 시군구 변수 추가하는 함수

의원 <- 의원 %>% 
  mutate(도로명전체주소 = ifelse(is.na(도로명전체주소), 소재지전체주소, 도로명전체주소)) %>%
  mutate(
    도 = str_extract(도로명전체주소, "([가-힣]+도)(?![가-힣0-9\\(])"),
    시 = str_extract(도로명전체주소, "([가-힣]+시)(?![가-힣0-9\\(])"),
    군 = str_extract(도로명전체주소, "([가-힣]+군)(?![가-힣0-9\\(])"),
    구 = str_extract(도로명전체주소, "([가-힣]+구)(?![가-힣0-9\\(])")
  ) %>%
  mutate(
    구 = ifelse(시 == "대구광역시", str_extract(도로명전체주소, "(?<=대구광역시[[:space:]])([가-힣]+구)"), 구)
  )

# 새로운 열에서 NA 값이 있는 경우 다음 열의 값을 가져와 채움

의원 <- 의원 %>% unite(시군구, 도, 시, 군, 구, sep = " ", na.rm = TRUE)


code <- read_excel("codecode.xlsx")

code <- distinct(code)
code$시도 <- sub("강원도", "강원특별자치도", code$시도)

#code <- code %>% mutate(code = paste(시도,시군구, sep = ""))
code <- code %>% mutate(시군구 = paste(시도,시군구, sep = " "))
code <- rename(code, code = 법정동코드)
code <- code %>% mutate(시군구 = ifelse(시군구 == "세종특별자치시 NA" , "세종특별자치시",시군구))

의원 <- left_join(의원, code, by = "시군구" )
의원 <- 의원 %>% filter(!(is.na(폐업일자) &  영업상태명 == "폐업"))
#병원 연도별
의원_2019 <- 의원 %>% filter(as.numeric(substr(인허가일자, 1, 4)) < 2020 & (as.numeric(substr(폐업일자, 1, 4)) > 2020 | is.na(폐업일자)))
의원_2023 <- 의원 %>% filter(as.numeric(substr(인허가일자, 1, 4)) < 2023 & (as.numeric(substr(폐업일자, 1, 4)) > 2024 | is.na(폐업일자)))



#업태구분명 분류
치과의원수 <- 의원 %>% filter(업태구분명 == "치과의원") %>% group_by(code) %>% summarise(치과의원수 = n())
한의원수 <- 의원 %>% filter(업태구분명 == "한의원") %>% group_by(code) %>% summarise(한의원수 = n())
일반의원수 <- 의원 %>% filter(업태구분명 == "의원") %>% group_by(code) %>% summarise(일반의원수 = n())

의원_도구수 <- 의원 %>%
  group_by(code) %>%
  summarise(의료인수 = sum(의료인수),입원실수 = sum(입원실수),병상수 = sum(병상수))
