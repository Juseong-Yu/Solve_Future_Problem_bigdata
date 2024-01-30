# 라이브러리 불러오기
library(readxl)

# 데이터 불러오기
library <- read_excel("data/library/전국도서관표준데이터.xls")
Sys.setlocale("LC_ALL", "en_US.UTF-8")
code <- read_excel("codecode.xlsx")

#데이터 정제
library <- library %>% 
  mutate(
    도 = str_extract(소재지도로명주소, "([가-힣]+도)"),
    시 = str_extract(소재지도로명주소, "([가-힣]+시)"),
    군 = str_extract(소재지도로명주소, "([가-힣]+군)"),
    구 = str_extract(소재지도로명주소, "([가-힣]+구)")
  ) %>%
  mutate(
    구 = ifelse(시 == "대구광역시", str_extract(소재지도로명주소, "(?<=대구광역시[[:space:]])([가-힣]+구)"), 구),
    도 = ifelse(도 == "강원도", "강원특별자치도", 도),
    시도명 = ifelse(시도명 == "강원도", "강원특별자치도", 시도명)
  )
library <- library %>% unite(시군구, 도, 시, 군, 구, sep = " ", na.rm = TRUE)

code <- distinct(code)
code$시도 <- sub("강원도", "강원특별자치도", code$시도)
#code <- code %>% mutate(code = paste(시도,시군구, sep = ""))
code <- code %>% mutate(시군구 = paste(시도,시군구, sep = " "))
code <- rename(code, code = 법정동코드)
code <- code %>% mutate(시군구 = ifelse(시군구 == "세종특별자치시 NA" , "세종특별자치시",시군구))
library <- left_join(library, code, by = "시군구" )

library <- library %>%
  mutate(
    시군구 = ifelse(is.na(code), ifelse(is.na(시군구명), 시도명, paste(시도명, 시군구명, sep = " ")), 시군구)
  ) %>%
  mutate(시군구 = ifelse(시군구 == "대구광역시 군위군", "경상북도 군위군", 시군구 ))

library <- left_join(library, code, by = "시군구" )
library <- library %>%
  rename(code = code.y)

library <- library %>%
  group_by(code) %>%
  summarise(도서관수 = n())
