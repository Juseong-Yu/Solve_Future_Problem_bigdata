# 라이브러리 불러오기
library(readxl)
library(dplyr)

# 데이터 받아오기
crime2022 <- read_excel("data/crime/범죄율.xlsx")
crime2019 <- read_excel("data/crime/범죄율2019.xlsx")

police <- read_excel("data/crime/경찰서.xlsx")
code <- read_excel("codecode.xlsx")

# 데이터 정제하기
police <- rename(police,
                 경찰서 = 경찰서명칭)

police$경찰서 <- gsub("경찰", "",police$경찰서)
crime2019$경찰서<- gsub("(경기|전북|충남|충북|전남|강원|경남|경북)", "", crime2019$경찰서)
crime2022$경찰서<- gsub("(경기|전북|충남|충북|전남|강원|경남|경북)", "", crime2022$경찰서)
police_crime2019 <- merge(crime2019, police, by = "경찰서",all = TRUE)
police_crime2022 <- merge(crime2022, police, by = "경찰서",all = TRUE)
police$경찰서 <- gsub("\\s", "", police$경찰서)

police_crime2019$새로운위치열 <- ifelse(grepl("특별시|광역시", police_crime2019$위치), police_crime2019$위치, sapply(strsplit(police_crime2019$위치, " "), function(x) x[1]))
police_crime2019$새로운경찰서주소 <- ifelse(grepl("시.*구", police_crime2019$경찰서주소), sapply(strsplit(police_crime2019$경찰서주소, " "), function(x) paste(x[1], x[2])), sapply(strsplit(police_crime2019$경찰서주소, " "), function(x) x[1]))
police_crime2019 <- police_crime2019 %>% mutate(시군구 = paste(새로운위치열, 새로운경찰서주소, sep = " "))

police_crime2022$새로운위치열 <- ifelse(grepl("특별시|광역시", police_crime2022$위치), police_crime2022$위치, sapply(strsplit(police_crime2022$위치, " "), function(x) x[1]))
police_crime2022$새로운경찰서주소 <- ifelse(grepl("시.*구", police_crime2022$경찰서주소), sapply(strsplit(police_crime2022$경찰서주소, " "), function(x) paste(x[1], x[2])), sapply(strsplit(police_crime2022$경찰서주소, " "), function(x) x[1]))
police_crime2022 <- police_crime2022 %>% mutate(시군구 = paste(새로운위치열, 새로운경찰서주소, sep = " "))

code <- distinct(code)
code$시도 <- sub("강원도", "강원특별자치도", code$시도)
#code <- code %>% mutate(code = paste(시도,시군구, sep = ""))
code <- code %>% mutate(시군구 = paste(시도,시군구, sep = " "))
code <- rename(code, code = 법정동코드)
police_crime2019 <- right_join(police_crime2019, code, by = "시군구" )
police_crime2022<- right_join(police_crime2022, code, by = "시군구" )

police_crime2019 <- police_crime2019 %>%
  group_by(code) %>%
  summarise(살인 = sum(살인), 강도 = sum(강도),절도 = sum(절도),폭력 = sum(폭력))

police_crime2022 <- police_crime2022 %>%
  group_by(code) %>%
  summarise(살인 = sum(살인), 강도 = sum(강도),절도 = sum(절도),폭력 = sum(폭력))

rm(police, crime2019,crime2022)