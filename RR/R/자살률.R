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


#인구십만명당 자살률


자살2019 <- read_excel("data/safe/인구십만명당_자살률_시도_2019.xlsx")
자살2023 <- read_excel("data/safe/인구십만명당_자살률_시도_2023.xlsx")
code <- read_excel("codecode.xlsx")

자살2019 <-자살2019  %>% unite(시군구, 시도,시군구, sep = " ", na.rm = TRUE)
자살2023 <-자살2023  %>% unite(시군구, 시도,시군구, sep = " ", na.rm = TRUE)

code <- distinct(code)
code$시도 <- sub("강원도", "강원특별자치도", code$시도)
#code <- code %>% mutate(code = paste(시도,시군구, sep = ""))
code <- code %>% mutate(시군구_tmp = paste(시도,시군구, sep = " "))
code <- rename(code, code = 법정동코드)
code <- code %>%
  mutate(시군구 = str_extract(시군구_tmp, "^[[:alnum:]]+\\s+[[:alnum:]]+"))
code <- code %>% mutate(시군구 = ifelse(시군구 == "세종특별자치시 NA" , "세종특별자치시",시군구))

자살2019 <- right_join(자살2019, code, by = "시군구" )
자살2023 <- right_join(자살2023, code, by = "시군구" )

자살2019 <- 자살2019 %>% select(code, 자살율)
자살2023 <- 자살2023 %>% select(code, 자살율)