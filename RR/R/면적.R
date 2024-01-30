# 라이브러리 불러오기
library(readxl)
library(dplyr)
library(tidyverse)

# 데이터 불러오기
면적 <- read_excel("data/면적_2022.xlsx",col_types = c("text", "text", "numeric"))
table(면적$면적)
# 데이터 정제하기
면적 <- 면적 %>% unite(시군구, 시도, 시군구, sep = " ", na.rm = TRUE)
면적 <- left_join(면적, code, by = "시군구")
