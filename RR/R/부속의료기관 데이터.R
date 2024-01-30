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



#부속의료기관

부속의료기관 <- read_excel("data/hospital/전국 부속의료기관 데이터.xls",col_types = c("numeric", "text", "text", 
                                                                     "numeric", "text", "date", "numeric", 
                                                                     "numeric", "text", "numeric", "text", 
                                                                     "date", "numeric", "numeric", "numeric", 
                                                                     "text", "numeric", "text", "text", 
                                                                     "text", "text", "text", "numeric", 
                                                                     "text", "numeric", "numeric", "numeric", 
                                                                     "numeric", "text", "numeric", "numeric", 
                                                                     "numeric", "numeric", "text", "text", 
                                                                     "numeric", "numeric", "numeric", 
                                                                     "numeric", "numeric", "numeric", 
                                                                     "numeric", "numeric", "numeric"))

#부속의료기관$영업상태명 <- ifelse(부속의료기관$영업상태명 == "영업/정상", 부속의료기관$영업상태명, NA)
#부속의료기관 <- 부속의료기관 %>% filter(!is.na(영업상태명))




# NEW 시군구 변수 추가하는 함수

부속의료기관 <- 부속의료기관 %>% 
  mutate(
    도 = str_extract(도로명전체주소, "([가-힣]+도)(?![가-힣0-9\\(])"),
    시 = str_extract(도로명전체주소, "([가-힣]+시)(?![가-힣0-9\\(])"),
    군 = str_extract(도로명전체주소, "([가-힣]+군)(?![가-힣0-9\\(])"),
    구 = str_extract(도로명전체주소, "([가-힣]+구)(?![가-힣0-9\\(])")
  ) %>%
  mutate(구 = ifelse(시 == "대구광역시", str_extract(도로명전체주소, "(?<=대구광역시[[:space:]])([가-힣]+구)"), 구),
         도 = ifelse(도 == "여의도","", 도)
  )


# 새로운 열에서 NA 값이 있는 경우 다음 열의 값을 가져와 채움

부속의료기관 <- 부속의료기관 %>% unite(시군구, 도, 시, 군, 구, sep = " ", na.rm = TRUE)



code <- read_excel("codecode.xlsx")

code <- distinct(code)
code$시도 <- sub("강원도", "강원특별자치도", code$시도)

#code <- code %>% mutate(code = paste(시도,시군구, sep = ""))
code <- code %>% mutate(시군구 = paste(시도,시군구, sep = " "))
code <- code %>% mutate(시군구 = ifelse(시군구 == "세종특별자치시 NA" , "세종특별자치시",시군구))
code <- rename(code, code = 법정동코드)

부속의료기관 <- inner_join(부속의료기관, code, by = "시군구" )

부속의료기관 <- 부속의료기관 %>% filter(!(is.na(폐업일자) &  영업상태명 == "폐업"))
부속의료기관_2019 <- 부속의료기관 %>% filter(as.numeric(substr(인허가일자, 1, 4)) < 2020 & (as.numeric(substr(폐업일자, 1, 4)) > 2020 | is.na(폐업일자)))
부속의료기관_2023 <- 부속의료기관 %>% filter(as.numeric(substr(인허가일자, 1, 4)) < 2020 & (as.numeric(substr(폐업일자, 1, 4)) > 2024 | is.na(폐업일자)))

# 2019년 데이터
# 도구 데이터

부속의료기관_도구수_2019 <- 부속의료기관_2019 %>%
  group_by(code) %>%
  summarise(의료인수 = sum(의료인수), 입원실수 = sum(입원실수),  병상수 = sum(병상수))

부속의료기관수_2019 <- 부속의료기관_2019 %>%
  group_by(code) %>%
  summarise(부속의료기관수 = n())
# 2019년 데이터
# 도구 데이터

부속의료기관_도구수_2023 <- 부속의료기관_2023 %>%
  group_by(code) %>%
  summarise(의료인수 = sum(의료인수), 입원실수 = sum(입원실수),  병상수 = sum(병상수))

부속의료기관수_2023 <- 부속의료기관_2023 %>%
  group_by(code) %>%
  summarise(부속의료기관수 = n())

rm(부속의료기관, 부속의료기관_2019,부속의료기관_2023)