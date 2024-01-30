library(dplyr)
library(stringr)
library(readr)
library(readxl)
library(tidyr)


#대기 데이터

초미세먼지_2019 <- read_excel("data/air/초미세먼지_PM2.5_2019.xlsx")
초미세먼지_2023 <- read_excel("data/air/초미세먼지_PM2.5_2023.xlsx")
미세먼지_2019 <- read_excel("data/air/미세먼지_PM10_2019.xlsx")
미세먼지_2023 <- read_excel("data/air/미세먼지_PM10_2023.xlsx")


#초미세먼지 2019
초미세먼지_2019 <- 초미세먼지_2019 %>%
  mutate(
    Y2019_1 = as.numeric(Y2019_1, na.rm = TRUE),
    Y2019_2 = as.numeric(Y2019_2, na.rm = TRUE),
    Y2019_3 = as.numeric(Y2019_3, na.rm = TRUE),
    Y2019_4 = as.numeric(Y2019_4, na.rm = TRUE),
    Y2019_5 = as.numeric(Y2019_5, na.rm = TRUE),
    Y2019_6 = as.numeric(Y2019_6, na.rm = TRUE),
    Y2019_7 = as.numeric(Y2019_7, na.rm = TRUE),
    Y2019_8 = as.numeric(Y2019_8, na.rm = TRUE),
    Y2019_9 = as.numeric(Y2019_9, na.rm = TRUE),
    Y2019_10 = as.numeric(Y2019_10, na.rm = TRUE),
    Y2019_11 = as.numeric(Y2019_11, na.rm = TRUE),
    Y2019_12 = as.numeric(Y2019_12, na.rm = TRUE),
  )


# '월' 열을 만들어서 연도와 월을 합칩니다.
초미세먼지_2019 <- 초미세먼지_2019 %>%
  mutate(
    연평균_초미세먼지 = rowMeans(select(., Y2019_1:Y2019_12), na.rm = TRUE)
  )

# NEW 시군구 변수 추가하는 함수
초미세먼지_2019$시군구 <- with(초미세먼지_2019, ifelse(!is.na(시군), paste(시도, 시군, sep = " "), 시도))




#초미세먼지 2023
초미세먼지_2023 <- 초미세먼지_2023 %>%
  mutate(
    Y2023_1 = as.numeric(Y2023_1, na.rm = TRUE),
    Y2023_2 = as.numeric(Y2023_2, na.rm = TRUE),
    Y2023_3 = as.numeric(Y2023_3, na.rm = TRUE),
    Y2023_4 = as.numeric(Y2023_4, na.rm = TRUE),
    Y2023_5 = as.numeric(Y2023_5, na.rm = TRUE)
  )


# '월' 열을 만들어서 연도와 월을 합칩니다.
초미세먼지_2023 <- 초미세먼지_2023 %>%
  mutate(
    연평균_초미세먼지 = rowMeans(select(., Y2023_1:Y2023_5), na.rm = TRUE)
  )




# NEW 시군구 변수 추가하는 함수
초미세먼지_2023$시군구 <- with(초미세먼지_2023, ifelse(!is.na(시군), paste(시도, 시군, sep = " "), 시도))






#미세먼지 2019
미세먼지_2019 <- 미세먼지_2019 %>%
  mutate(
    Y2019_1 = as.numeric(Y2019_1, na.rm = TRUE),
    Y2019_2 = as.numeric(Y2019_2, na.rm = TRUE),
    Y2019_3 = as.numeric(Y2019_3, na.rm = TRUE),
    Y2019_4 = as.numeric(Y2019_4, na.rm = TRUE),
    Y2019_5 = as.numeric(Y2019_5, na.rm = TRUE),
    Y2019_6 = as.numeric(Y2019_6, na.rm = TRUE),
    Y2019_7 = as.numeric(Y2019_7, na.rm = TRUE),
    Y2019_8 = as.numeric(Y2019_8, na.rm = TRUE),
    Y2019_9 = as.numeric(Y2019_9, na.rm = TRUE),
    Y2019_10 = as.numeric(Y2019_10, na.rm = TRUE),
    Y2019_11 = as.numeric(Y2019_11, na.rm = TRUE),
    Y2019_12 = as.numeric(Y2019_12, na.rm = TRUE),
  )

# '월' 열을 만들어서 연도와 월을 합칩니다.
미세먼지_2019 <- 미세먼지_2019 %>%
  mutate(
    연평균_미세먼지 = rowMeans(select(., Y2019_1:Y2019_12), na.rm = TRUE)
  )

# NEW 시군구 변수 추가하는 함수
미세먼지_2019$시군구 <- with(미세먼지_2019, ifelse(!is.na(시군), paste(시도, 시군, sep = " "), 시도))



#미세먼지 2023
미세먼지_2023 <- 미세먼지_2023 %>%
  mutate(
    Y2023_1 = as.numeric(Y2023_1, na.rm = TRUE),
    Y2023_2 = as.numeric(Y2023_2, na.rm = TRUE),
    Y2023_3 = as.numeric(Y2023_3, na.rm = TRUE),
    Y2023_4 = as.numeric(Y2023_4, na.rm = TRUE),
    Y2023_5 = as.numeric(Y2023_5, na.rm = TRUE)
  )


# '월' 열을 만들어서 연도와 월을 합칩니다.
미세먼지_2023 <- 미세먼지_2023 %>%
  mutate(
    연평균_미세먼지 = rowMeans(select(., Y2023_1:Y2023_5), na.rm = TRUE)
  )




# NEW 시군구 변수 추가하는 함수
미세먼지_2023$시군구 <- with(미세먼지_2023, ifelse(!is.na(시군), paste(시도, 시군, sep = " "), 시도))





code <- read_excel("codecode.xlsx")

code <- distinct(code)
code$시도 <- sub("강원도", "강원특별자치도", code$시도)

#code <- code %>% mutate(code = paste(시도,시군구, sep = ""))
code <- code %>% mutate(시군구_tmp = paste(시도,시군구, sep = " "))
code <- code %>% mutate(시군구 = paste(시도,시군구, sep = " "))
code <- rename(code, code = 법정동코드)
code <- code %>%
  mutate(시군구 = str_extract(시군구_tmp, "^[[:alnum:]]+\\s+[[:alnum:]]+"))
code <- code %>%
  mutate(시군구 = sub("(서울특별시|부산광역시|대구광역시|인천광역시|광주광역시|대전광역시|울산광역시)\\s.*", "\\1", 시군구))
code <- code %>% mutate(시군구 = ifelse(시군구 == "세종특별자치시 NA" , "세종특별자치시",시군구))

초미세먼지_2019 <- left_join(초미세먼지_2019, code, by = "시군구" )
초미세먼지_2023 <- left_join(초미세먼지_2023, code, by = "시군구" )
미세먼지_2019 <- left_join(미세먼지_2019, code, by = "시군구" )
미세먼지_2023 <- left_join(미세먼지_2023, code, by = "시군구" )

초미세먼지_2019 <-select(초미세먼지_2019, 연평균_초미세먼지, code)
초미세먼지_2023 <-select(초미세먼지_2019, 연평균_초미세먼지, code)
미세먼지_2019 <-select(미세먼지_2019, 연평균_미세먼지, code)
미세먼지_2023 <-select(미세먼지_2023, 연평균_미세먼지, code)