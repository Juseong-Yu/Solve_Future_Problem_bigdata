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


사회복지시설2019 <- read_excel("data/culture/인구_십만명당_사회복지시설수_2019.xlsx")
사회복지시설2023 <- read_excel("data/culture/인구_십만명당_사회복지시설수_2023.xlsx")
code <- read_excel("codecode.xlsx")

사회복지시설2019 <-사회복지시설2019  %>% unite(시군구, 시도,시군구, sep = " ", na.rm = TRUE)
사회복지시설2023 <-사회복지시설2023  %>% unite(시군구, 시도,시군구, sep = " ", na.rm = TRUE)

code <- distinct(code)
code$시도 <- sub("강원도", "강원특별자치도", code$시도)
#code <- code %>% mutate(code = paste(시도,시군구, sep = ""))
code <- code %>% mutate(시군구_tmp = paste(시도,시군구, sep = " "))
code <- rename(code, code = 법정동코드)
code <- code %>%
  mutate(시군구 = str_extract(시군구_tmp, "^[[:alnum:]]+\\s+[[:alnum:]]+"))
code <- code %>% mutate(시군구 = ifelse(시군구 == "세종특별자치시 NA" , "세종특별자치시",시군구))

사회복지시설2019 <- right_join(사회복지시설2019, code, by = "시군구" )
사회복지시설2023 <- right_join(사회복지시설2023, code, by = "시군구" )

사회복지시설2019 <- 사회복지시설2019 %>% select(code, 인구십만명당_사회복지시설수)
사회복지시설2023 <- 사회복지시설2023 %>% select(code, 인구십만명당_사회복지시설수)
