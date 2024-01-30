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


#병원

병원 <- read_excel("data/hospital/전국 병원 데이터.xls",col_types = c("numeric", "text", "text", 
                                                             "numeric", "text", "date", "numeric", 
                                                             "numeric", "text", "numeric", "text", 
                                                             "date", "numeric", "numeric", "numeric", 
                                                             "text", "numeric", "text", "text", 
                                                             "text", "text", "text", "numeric", 
                                                             "text", "numeric", "text", "numeric", 
                                                             "numeric", "text", "numeric", "numeric", 
                                                             "numeric", "numeric", "text", "text", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "text"))


#병원$영업상태명 <- ifelse(병원$영업상태명 == "영업/정상", 병원$영업상태명, NA)
#병원 <- 병원 %>% filter(!is.na(영업상태명))

#병원 <- mutate(병원, 시군구 = sapply(도로명전체주소, convert_to_district_or_city_and_last_word))


# NEW 시군구 변수 추가하는 함수
병원 <- 병원 %>% 
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

병원 <- 병원 %>% unite(시군구, 도, 시, 군, 구, sep = " ", na.rm = TRUE)

code <- read_excel("codecode.xlsx")

code <- distinct(code)
code$시도 <- sub("강원도", "강원특별자치도", code$시도)

#code <- code %>% mutate(code = paste(시도,시군구, sep = ""))
code <- code %>% mutate(시군구 = paste(시도,시군구, sep = " "))
code <- code %>% mutate(시군구 = ifelse(시군구 == "세종특별자치시 NA" , "세종특별자치시",시군구))
code <- rename(code, code = 법정동코드)

병원 <- left_join(병원, code, by = "시군구" )
병원 <- 병원 %>% filter(!(is.na(폐업일자) &  영업상태명 == "폐업"))
#병원 연도별
병원_2019 <- 병원 %>% filter(as.numeric(substr(인허가일자, 1, 4)) < 2020 & (as.numeric(substr(폐업일자, 1, 4)) > 2020 | is.na(폐업일자)))
병원_2023 <- 병원 %>% filter(as.numeric(substr(인허가일자, 1, 4)) < 2023 & (as.numeric(substr(폐업일자, 1, 4)) > 2024 | is.na(폐업일자)))

# 업태구분 - "요양병원(일반요양병원)", "정신병원", "병원", "요양병원(노인병원)", "종합병원", "한방병원",  "치과병원"              

#unique_업태구분_2019 <- unique(병원2019$업태구분명)
#unique_업태구분

# 2019년 데이터
요양병원_일반요양병원수_2019 <- 병원_2019 %>% filter(업태구분명 == "요양병원(일반요양병원)")
요양병원_일반요양병원수_2019 <- 요양병원_일반요양병원수_2019 %>%
  group_by(code) %>%
  summarise(일반요양병원수 = n())

일반병원수_2019 <- 병원_2019 %>% filter(업태구분명 == "병원")
일반병원수_2019 <- 일반병원수_2019 %>%
  group_by(code) %>%
  summarise(일반병원수 = n())

요양병원_노인병원수_2019 <- 병원_2019 %>% filter(업태구분명 == "요양병원(노인병원)")
요양병원_노인병원수_2019 <- 요양병원_노인병원수_2019 %>%
  group_by(code) %>%
  summarise(노인병원수 = n())

종합병원수_2019 <- 병원_2019 %>% filter(업태구분명 == "종합병원")
종합병원수_2019 <- 종합병원수_2019 %>%
  group_by(code) %>%
  summarise(종합병원수 = n())

한방병원수_2019 <- 병원_2019 %>% filter(업태구분명 == "한방병원")
한방병원수_2019 <- 한방병원수_2019 %>%
  group_by(code) %>%
  summarise(한방병원수 = n())

치과병원수_2019 <- 병원_2019 %>% filter(업태구분명 == "치과병원")
치과병원수_2019 <- 치과병원수_2019 %>%
  group_by(code) %>%
  summarise(치과병원수 = n())


# 도구 데이터
병원_도구수_2019 <- 병원_2019 %>%
  group_by(code) %>%
  summarise(의료인수 = sum(의료인수),입원실수 = sum(입원실수),병상수 = sum(병상수))

# 2023년 데이터
요양병원_일반요양병원수_2023 <- 병원_2023 %>% filter(업태구분명 == "요양병원(일반요양병원)")
요양병원_일반요양병원수_2023 <- 요양병원_일반요양병원수_2023 %>%
  group_by(code) %>%
  summarise(일반요양병원수 = n())

일반병원수_2023 <- 병원_2023 %>% filter(업태구분명 == "병원")
일반병원수_2023 <- 일반병원수_2023 %>%
  group_by(code) %>%
  summarise(일반병원수 = n())

요양병원_노인병원수_2023 <- 병원_2023 %>% filter(업태구분명 == "요양병원(노인병원)")
요양병원_노인병원수_2023 <- 요양병원_노인병원수_2023 %>%
  group_by(code) %>%
  summarise(노인병원수 = n())

종합병원수_2023 <- 병원_2023 %>% filter(업태구분명 == "종합병원")
종합병원수_2023 <- 종합병원수_2023 %>%
  group_by(code) %>%
  summarise(종합병원수 = n())

한방병원수_2023 <- 병원_2023 %>% filter(업태구분명 == "한방병원")
한방병원수_2023 <- 한방병원수_2023 %>%
  group_by(code) %>%
  summarise(한방병원수 = n())

치과병원수_2023 <- 병원_2023 %>% filter(업태구분명 == "치과병원")
치과병원수_2023 <- 치과병원수_2023 %>%
  group_by(code) %>%
  summarise(치과병원수 = n())


# 도구 데이터
병원_도구수_2023 <- 병원_2023 %>%
  group_by(code) %>%
  summarise(의료인수 = sum(의료인수),입원실수 = sum(입원실수),병상수 = sum(병상수))

rm(병원, 병원_2019, 병원_2023)
