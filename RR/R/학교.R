#라이브러리 불러오기
library(readxl)
library(dplyr)
library(stringr)
library(tidyverse)

#데이터 받아오기
경기도학교 <- read_excel("data/school/경기도학교.xls")
경상남도학교 <- read_excel("data/school/경상남도학교.xls")
경상북도학교 <- read_excel("data/school/경상북도학교.xls")
광주학교 <- read_excel("data/school/광주학교.xls")
대구학교 <- read_excel("data/school/대구학교.xls")
대전학교 <- read_excel("data/school/대전학교.xls")
부산학교 <- read_excel("data/school/부산학교.xls")
서울학교 <- read_excel("data/school/서울학교.xls")
세종학교 <- read_excel("data/school/세종학교.xls")
울산학교 <- read_excel("data/school/울산학교.xls")
인천학교 <- read_excel("data/school/인천학교.xls")
전라남도학교 <- read_excel("data/school/전라남도학교.xls")
전라북도학교 <- read_excel("data/school/전라북도학교.xls")
제주도학교 <- read_excel("data/school/제주도학교.xls")
충청남도학교 <- read_excel("data/school/충청남도학교.xls")
충청북도학교 <- read_excel("data/school/충청북도학교.xls")
강원도학교 <- read_excel("data/school/강원도학교.xls")

학교 <- bind_rows(경기도학교,경상남도학교,경상북도학교,광주학교,대구학교,대전학교,
                부산학교,서울학교,세종학교,울산학교,인천학교,전라남도학교,전라북도학교,
                제주도학교,충청남도학교,충청북도학교,강원도학교)

rm(경기도학교,경상남도학교,경상북도학교,광주학교,대구학교,대전학교,
   부산학교,서울학교,세종학교,울산학교,인천학교,전라남도학교,전라북도학교,
   제주도학교,충청남도학교,충청북도학교,강원도학교)

code <- read_excel("codecode.xlsx")

#데이터 정제하기
학교 <- 학교 %>% 
  mutate(
    도 = str_extract(도로명주소, "([가-힣]+도)(?![가-힣0-9])"),
    시 = str_extract(도로명주소, "([가-힣]+시)(?![가-힣0-9])"),
    군 = str_extract(도로명주소, "([가-힣]+군)(?![가-힣0-9])"),
    구 = str_extract(도로명주소, "([가-힣]+구)(?![가-힣0-9])")
  ) %>%
  mutate(
    구 = ifelse(시 == "대구광역시", str_extract(도로명주소, "(?<=대구광역시[[:space:]])([가-힣]+구)"), 구),
    도 = ifelse(도 == "강원도" , "강원특별자치도", 도)
  )

학교 <- 학교 %>% unite(시군구, 도, 시, 군, 구, sep = " ", na.rm = TRUE)

code <- distinct(code)
code$시도 <- sub("강원도", "강원특별자치도", code$시도)
#code <- code %>% mutate(code = paste(시도,시군구, sep = ""))
code <- code %>% mutate(시군구 = paste(시도,시군구, sep = " "))
code <- rename(code, code = 법정동코드)
code <- code %>% mutate(시군구 = ifelse(시군구 == "세종특별자치시 NA" , "세종특별자치시",시군구))

학교 <- inner_join(학교, code, by = "시군구" )

#학교별 분류
table(학교$학교종류명)
고등학교 <- 학교 %>% filter(학교종류명 == "각종학교(고)"| 학교종류명 == "고등공민학교" | 학교종류명 == "고등기술학교" | 학교종류명 == "고등학교" |
                        학교종류명 == "방송통신고등학교" | 학교종류명 == "평생학교(고)-2년6학기" | 학교종류명 == "평생학교(고)-3년6학기")

중학교 <- 학교 %>% filter(학교종류명 == "각종학교(중)"| 학교종류명 == "중학교" | 학교종류명 == "방송통신중학교" | 학교종류명 == "평생학교(중)-2년6학기" | 학교종류명 =="평생학교(중)-3년6학기")

초등학교 <- 학교 %>% filter(학교종류명 == "각종학교(초)"| 학교종류명 == "초등학교" | 학교종류명 == "평생학교(초)-3년6학기" | 학교종류명 =="평생학교(초)-4년12학기")

특수학교 <- 학교 %>% filter(학교종류명 == "특수학교")

외국인_국제학교 <- 학교 %>% filter(학교종류명 == "각종학교(외국인학교)"| 학교종류명 == "국제학교 " | 학교종류명 == "외국인학교")

#도시별로 카운트
고등학교 <- 고등학교 %>% 
  group_by(code) %>%
  summarise(고등학교수 = n())

중학교 <- 중학교 %>% 
  group_by(code) %>%
  summarise(중학교수 = n())

초등학교 <- 초등학교 %>% 
  group_by(code) %>%
  summarise(초등학교수 = n())

특수학교 <- 특수학교 %>% 
  group_by(code) %>%
  summarise(특수학교수 = n())

외국인_국제학교 <- 외국인_국제학교 %>% 
  group_by(code) %>%
  summarise(외국인_국제학교 = n())

rm(학교)
