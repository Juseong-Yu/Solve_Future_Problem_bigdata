# 라이브러리 불러오기
install.packages("readxl")
install.packages("stringr")
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyverse)

# 데이터 불러오기
seoul_kinder <- read_excel("data/kindergarden/서울시_어린이집_현황_202411.xls")
gandwon_kinder <- read_excel("data/kindergarden/강원도_어린이집_현황_202411.xls")
geongi_kinder <- read_excel("data/kindergarden/경기도_어린이집_현황_202411.xls")
geongsangnam_kinder <- read_excel("data/kindergarden/경상남도_어린이집_현황_202411.xls")
geongsamgebuk_kinder <- read_excel("data/kindergarden/경상북도_어린이집_현황_202411.xls")
daegu_kinder <- read_excel("data/kindergarden/대구시_어린이집_현황_202411.xls")
daejeon_kinder <- read_excel("data/kindergarden/대전_어린이집_현황_202411.xls")
guangju_kinder <- read_excel("data/kindergarden/광주_어린이집_현황_202411.xls")
busan_kinder <- read_excel("data/kindergarden/부산시_어린이집_현황_202411.xls")
seojong_kinder <- read_excel("data/kindergarden/세종시_어린이집_현황_202411.xls")
ulsan_kinder <- read_excel("data/kindergarden/울산_어린이집_현황_202411.xls")
incheon_kinder <- read_excel("data/kindergarden/인천_어린이집_현황_202411.xls")
jeonlabuk_kinder <- read_excel("data/kindergarden/전라북도_어린이집_현황_202411.xls")
jeju_kinder <- read_excel("data/kindergarden/제주도_어린이집_현황_202411.xls")
chongchungnam_kinder <- read_excel("data/kindergarden/충청남도_어린이집_현황_202411.xls")
chongchungbuk_kinder <- read_excel("data/kindergarden/충청북도_어린이집_현황_202411.xls")
jeunlanam_kinder <- read_excel("data/kindergarden/전라남도_어린이집_현황_202411.xls")

seoul_kinder2019 <- read_excel("data/kindergarden/서울시_어린이집_현황_201901.xls")
gandwon_kinder2019 <- read_excel("data/kindergarden/강원도_어린이집_현황_201901.xls")
geongi_kinder2019 <- read_excel("data/kindergarden/경기도_어린이집_현황_201901.xls")
geongsangnam_kinder2019 <- read_excel("data/kindergarden/경상남도_어린이집_현황_201901.xls")
geongsamgebuk_kinder2019 <- read_excel("data/kindergarden/경상북도_어린이집_현황_201901.xls")
daegu_kinder2019 <- read_excel("data/kindergarden/대구시_어린이집_현황_201901.xls")
daejeon_kinder2019 <- read_excel("data/kindergarden/대전_어린이집_현황_201901.xls")
guangju_kinder2019 <- read_excel("data/kindergarden/광주_어린이집_현황_201901.xls")
busan_kinder2019 <- read_excel("data/kindergarden/부산시_어린이집_현황_201901.xls")
seojong_kinder2019 <- read_excel("data/kindergarden/세종시_어린이집_현황_201901.xls")
ulsan_kinder2019 <- read_excel("data/kindergarden/울산_어린이집_현황_201901.xls")
incheon_kinder2019 <- read_excel("data/kindergarden/인천_어린이집_현황_201901.xls")
jeonlabuk_kinder2019 <- read_excel("data/kindergarden/전라북도_어린이집_현황_201901.xls")
jeju_kinder2019 <- read_excel("data/kindergarden/제주도_어린이집_현황_201901.xls")
chongchungnam_kinder2019 <- read_excel("data/kindergarden/충청남도_어린이집_현황_201901.xls")
chongchungbuk_kinder2019 <- read_excel("data/kindergarden/충청북도_어린이집_현황_201901.xls")
jeunlanam_kinder2019 <- read_excel("data/kindergarden/전라남도_어린이집_현황_201901.xls")

all_kinder2023 <- bind_rows(seoul_kinder, gandwon_kinder, geongi_kinder,geongsangnam_kinder,
                       geongsamgebuk_kinder, daegu_kinder, daejeon_kinder, guangju_kinder,
                       busan_kinder,seojong_kinder,ulsan_kinder,incheon_kinder,jeonlabuk_kinder,
                       jeju_kinder,chongchungnam_kinder,chongchungbuk_kinder,jeunlanam_kinder)
rm(seoul_kinder, gandwon_kinder, geongi_kinder,geongsangnam_kinder,
    geongsamgebuk_kinder, daegu_kinder, daejeon_kinder, guangju_kinder,
    busan_kinder,seojong_kinder,ulsan_kinder,incheon_kinder,jeonlabuk_kinder,
    jeju_kinder,chongchungnam_kinder,chongchungbuk_kinder,jeunlanam_kinder)

all_kinder2019 <- bind_rows(seoul_kinder2019, gandwon_kinder2019, geongi_kinder2019,geongsangnam_kinder2019,
                            geongsamgebuk_kinder2019, daegu_kinder2019, daejeon_kinder2019, guangju_kinder2019,
                            busan_kinder2019,seojong_kinder2019,ulsan_kinder2019,incheon_kinder2019,jeonlabuk_kinder2019,
                            jeju_kinder2019,chongchungnam_kinder2019,chongchungbuk_kinder2019,jeunlanam_kinder2019)
rm(seoul_kinder2019, gandwon_kinder2019, geongi_kinder2019,geongsangnam_kinder2019,
   geongsamgebuk_kinder2019, daegu_kinder2019, daejeon_kinder2019, guangju_kinder2019,
   busan_kinder2019,seojong_kinder2019,ulsan_kinder2019,incheon_kinder2019,jeonlabuk_kinder2019,
   jeju_kinder2019,chongchungnam_kinder2019,chongchungbuk_kinder2019,jeunlanam_kinder2019)

code <- read_excel("codecode.xlsx")

# 데이터 정제하기

all_kinder2023$운영현황 <- ifelse(all_kinder2023$운영현황 == "정상" | all_kinder2023$운영현황 == "재개", all_kinder2023$운영현황, NA)
all_kinder2023 <- all_kinder2023 %>% filter(!is.na(운영현황))

all_kinder2019$운영현황 <- ifelse(all_kinder2019$운영현황 == "정상"| all_kinder2019$운영현황 == "재개", all_kinder2019$운영현황, NA)
all_kinder2019 <- all_kinder2019 %>% filter(!is.na(운영현황))

all_kinder2023 <- all_kinder2023 %>% 
  mutate(
    도 = str_extract(주소, "([가-힣]+도)(?![가-힣0-9\\(])"),
    시 = str_extract(주소, "([가-힣]+시)(?![가-힣0-9\\(])"),
    군 = str_extract(주소, "([가-힣]+군)(?![가-힣0-9\\(])"),
    구 = str_extract(주소, "([가-힣]+구)(?![가-힣0-9\\(])")
  ) %>%
  mutate(
    구 = ifelse(시 == "대구광역시", str_extract(주소, "(?<=대구광역시[[:space:]])([가-힣]+구)"), 구),
    도 = ifelse(도 == "강원도", "강원특별자치도", 도),
    도 = ifelse(도 == "전북특별자치도", "전라북도", 도)
  )

all_kinder2023 <- all_kinder2023 %>% unite(시군구, 도, 시, 군, 구, sep = " ", na.rm = TRUE)

all_kinder2019 <- all_kinder2019 %>% 
  mutate(
    도 = str_extract(주소, "([가-힣]+도)(?![가-힣0-9\\(])"),
    시 = str_extract(주소, "([가-힣]+시)(?![가-힣0-9\\(])"),
    군 = str_extract(주소, "([가-힣]+군)(?![가-힣0-9\\(])"),
    구 = str_extract(주소, "([가-힣]+구)(?![가-힣0-9\\(])")
  ) %>%
  mutate(
    구 = ifelse(시 == "대구광역시", str_extract(주소, "(?<=대구광역시[[:space:]])([가-힣]+구)"), 구),
    구 = ifelse(구 == "오정구" | 구 == "원미구" | 구 == "소사구", NA, 구),
    도 = ifelse(도 == "강원도", "강원특별자치도", 도),
    도 = ifelse(도 == "전북특별자치도", "전라북도", 도)
  )

all_kinder2019 <- all_kinder2019 %>% unite(시군구, 도, 시, 군, 구, sep = " ", na.rm = TRUE)

code <- distinct(code)
code$시도 <- sub("강원도", "강원특별자치도", code$시도)
#code <- code %>% mutate(code = paste(시도,시군구, sep = ""))
code <- code %>% mutate(시군구 = paste(시도,시군구, sep = " "))
code <- code %>% mutate(시군구 = ifelse(시군구 == "세종특별자치시 NA" , "세종특별자치시",시군구))
code <- rename(code, code = 법정동코드)

all_kinder2023 <- left_join(all_kinder2023, code, by = "시군구" )
all_kinder2019 <- left_join(all_kinder2019, code, by = "시군구" )

all_kinder2019 <- all_kinder2019 %>%
  mutate(시군구 = ifelse(is.na(code),paste(word(주소, 1), word(주소, 2), sep = " "),시군구))
all_kinder2023 <- all_kinder2023 %>%
  mutate(시군구 = ifelse(is.na(code),paste(word(주소, 1), word(주소, 2), sep = " "),시군구))

all_kinder2019 <- inner_join(all_kinder2019, code, by = "시군구" )
all_kinder2023 <- inner_join(all_kinder2023, code, by = "시군구" )

all_kinder2019 <- rename(all_kinder2019, code = code.y)
all_kinder2023 <- rename(all_kinder2023, code = code.y)

all_kinder2019 <- all_kinder2019 %>%
  group_by(code) %>%
  summarise(어린이집수 = n())

all_kinder2023 <- all_kinder2023 %>%
  group_by(code) %>%
  summarise(어린이집수 = n())
