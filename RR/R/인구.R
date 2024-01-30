# 라이브러리 설정
install.packages("stringi")
library(readxl)
library(dplyr)
library(stringr)

#데이터 가져오기
population_2023 <- read_excel("data/202312_202312_연령별인구현황_월간.xlsx")
population_2019 <- read_excel("data/201912_201912_연령별인구현황_월간.xlsx")
code <- read_excel("codecode.xlsx")

# 시군구 코드 정리
code <- distinct(code)
code$시도 <- sub("강원도", "강원특별자치도", code$시도)
#code <- code %>% mutate(code = paste(시도,시군구, sep = ""))
code <- code %>% mutate(시군구 = paste(시도,시군구, sep = " "))
code <- rename(code, code = 법정동코드)
code <- code %>% mutate(시군구 = ifelse(시군구 == "세종특별자치시 NA" , "세종특별자치시",시군구))

population_2023 <- rename(population_2023, 시군구 = 행정기관)
population_2019 <- rename(population_2019, 시군구 = 행정기관)

#값 병합
remove_comma <- function(x) {
  as.numeric(gsub(",", "", x))
}

population_2023 <- right_join(population_2023, code, by = "시군구" )
population_2023$총인구수 <- remove_comma(population_2023$총인구수)
population_2023$code <- as.numeric(population_2023$code)

population_2019 <- right_join(population_2019, code, by = "시군구" )
population_2019$총인구수 <- remove_comma(population_2019$총인구수)
population_2019$code <- as.numeric(population_2019$code)

population_2019 <- select(population_2019, code, 총인구수)
population_2023 <- select(population_2023, code, 총인구수)



#0세 인구
born2019 <- read_excel("data/2019_0세인구.xlsx")
born2020 <- read_excel("data/2020_0세인구.xlsx")
born2021 <- read_excel("data/2021_0세인구.xlsx")
born2022 <- read_excel("data/2022_0세인구.xlsx")
born2023 <- read_excel("data/2023_0세인구.xlsx")

born2019 <- rename(born2019, 시군구 = 행정기관)
born2020 <- rename(born2020, 시군구 = 행정기관)
born2021 <- rename(born2021, 시군구 = 행정기관)
born2022 <- rename(born2022, 시군구 = 행정기관)
born2023 <- rename(born2023, 시군구 = 행정기관)

born2019 <- right_join(born2019, code, by = "시군구" )
born2019$zero <- remove_comma(born2019$zero)
born2019$code <- as.numeric(born2019$code)

born2020 <- right_join(born2020, code, by = "시군구" )
born2020$zero <- remove_comma(born2020$zero)
born2020$총인구수 <- remove_comma(born2020$총인구수)
born2020$code <- as.numeric(born2020$code)

born2021 <- right_join(born2021, code, by = "시군구" )
born2021$zero <- remove_comma(born2021$zero)
born2021$총인구수 <- remove_comma(born2021$총인구수)
born2021$code <- as.numeric(born2021$code)

born2022 <- right_join(born2022, code, by = "시군구" )
born2022$zero <- remove_comma(born2022$zero)
born2022$총인구수 <- remove_comma(born2022$총인구수)
born2022$code <- as.numeric(born2022$code)

born2023 <- right_join(born2023, code, by = "시군구" )
born2023$zero <- remove_comma(born2023$zero)
born2023$code <- as.numeric(born2023$code)


population_2020 <- select(born2020, code, 총인구수)
population_2021 <-select(born2021, code, 총인구수)
population_2022<-select(born2022, code, 총인구수)

born2019 <- select(born2019, code, zero)
born2020<- select(born2020, code,zero)
born2021<- select(born2021, code,zero)
born2022<- select(born2022, code,zero)
born2023<- select(born2023, code,zero)
