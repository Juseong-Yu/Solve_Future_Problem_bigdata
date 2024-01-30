library(readxl)
code <- read_excel("codecode.xlsx")

# 시군구 코드 정리
code <- distinct(code)
code$시도 <- sub("강원도", "강원특별자치도", code$시도)
#code <- code %>% mutate(code = paste(시도,시군구, sep = ""))
code <- code %>% mutate(시군구 = paste(시도,시군구, sep = " "))
code <- rename(code, code = 법정동코드)
code <- code %>% mutate(시군구 = ifelse(시군구 == "세종특별자치시 NA" , "세종특별자치시",시군구))
code <- code %>% select(시군구, code)

#2019년 데이터 취합 
data_2019 <- right_join(code, 교원_학생수2019, by = "code")
data_2019 <- left_join(data_2019, 학생수2019, by = "code")
data_2019 <- left_join(data_2019, all_kinder2019, by = "code")
data_2019 <- left_join(data_2019, 고등학교, by = "code")
data_2019 <- left_join(data_2019, 중학교, by = "code")
data_2019 <- left_join(data_2019, 초등학교, by = "code")
data_2019 <- left_join(data_2019, 특수학교, by = "code")
data_2019 <- left_join(data_2019, 외국인_국제학교, by = "code")
data_2019 <- left_join(data_2019, library, by = "code")
data_2019 <- left_join(data_2019, 요양병원_일반요양병원수_2019, by = "code")
data_2019 <- left_join(data_2019, 요양병원_노인병원수_2019, by = "code")
data_2019 <- left_join(data_2019, 일반병원수_2019, by = "code")
data_2019 <- left_join(data_2019, 종합병원수_2019, by = "code")
data_2019 <- left_join(data_2019, 한방병원수_2019, by = "code")
data_2019 <- left_join(data_2019, 치과병원수_2019, by = "code")
data_2019 <- left_join(data_2019, 병원_도구수_2019, by = "code")
data_2019 <- left_join(data_2019, 부속의료기관수_2019, by = "code")
data_2019 <- left_join(data_2019, 부속의료기관_도구수_2019, by = "code")
data_2019 <- left_join(data_2019, 치과의원수, by = "code")
data_2019 <- left_join(data_2019, 한의원수, by = "code")
data_2019 <- left_join(data_2019, 일반의원수, by = "code")
data_2019 <- left_join(data_2019, 의원_도구수, by = "code")
data_2019 <- left_join(data_2019, 경찰서, by = "code")
data_2019 <- left_join(data_2019, 소방서, by = "code")
data_2019 <- left_join(data_2019, 초미세먼지_2019, by = "code")
data_2019 <- left_join(data_2019, 미세먼지_2019, by = "code")
data_2019 <- left_join(data_2019, 문화기반시설2019, by = "code")
data_2019 <- left_join(data_2019, 사회복지시설2019, by = "code")
data_2019 <- left_join(data_2019, 어린이_교통사고2019, by = "code")
data_2019 <- left_join(data_2019, 노인_교통사고2019, by = "code")
data_2019 <- left_join(data_2019, 자살2019, by = "code")
data_2019 <- left_join(data_2019, population_2019, by = "code")
data_2019 <- left_join(data_2019, born2019, by = "code")

data_2019[is.na(data_2019)] <- 0
data_2019 <- data_2019 %>% mutate(의료인수 = 의료인수 + 의료인수.x+ 의료인수.y)
data_2019 <- data_2019 %>% mutate(입원실수 = 입원실수 + 입원실수.x + 입원실수.y)
data_2019 <- data_2019 %>% mutate(의료인수 = 병상수 + 병상수.x + 병상수.y)
data_2019 <- select(data_2019, -의료인수.x, -의료인수.y, -입원실수.x, -입원실수.y, -병상수.x, -병상수.y) 

# 2019년 데이터 다중회귀로 확인
model <- lm(총인구수 ~ 교원1인당_학생수 + 학급당_유치원 + 학급당_초등학교 + 학급당_고등학교 +어린이집수+ 고등학교수+
              중학교수 + 초등학교수 + 특수학교수+외국인_국제학교 + 도서관수 +일반요양병원수+ 노인병원수+일반병원수
            +종합병원수+한방병원수+치과병원수+부속의료기관수+치과의원수+한의원수+일반의원수+입원실수+의료인수+병상수+경찰서+소방서+연평균_초미세먼지+연평균_미세먼지
            +인구십만명당_문화기반시설수 + 인구십만명당_사회복지시설수 + 자살율 + 사고건수_어 + 사고건수_노,data = data_2019)
summary(model)

model <- lm(zero ~ 교원1인당_학생수 + 학급당_유치원 + 학급당_초등학교 + 학급당_고등학교 +어린이집수+ 도서관수 + 고등학교수+
              중학교수 + 초등학교수 + 특수학교수+외국인_국제학교 + 일반요양병원수+ 노인병원수+일반병원수
            +종합병원수+한방병원수+치과병원수+부속의료기관수+치과의원수+한의원수+일반의원수+입원실수 + 의료인수+병상수+경찰서+소방서+연평균_초미세먼지+연평균_미세먼지
            +인구십만명당_문화기반시설수 + 인구십만명당_사회복지시설수 + 자살율 + 사고건수_어 + 사고건수_노,data = data_2019)
summary(model)

# 2019년 데이터 정규화 (min-max 정규화 활용)
normalized_2019 <- select(data_2019, code, 시군구.x,교원1인당_학생수, 학급당_유치원, 학급당_초등학교, 학급당_고등학교, 학급당_중학교, 어린이집수,도서관수,
                           고등학교수,
                            중학교수 , 초등학교수 , 특수학교수,외국인_국제학교 ,
                          일반요양병원수, 노인병원수, 일반병원수
                          ,종합병원수, 한방병원수, 치과병원수,부속의료기관수, 치과의원수, 한의원수, 일반의원수, 입원실수 , 의료인수, 병상수, 경찰서, 소방서, 연평균_초미세먼지, 
                          ,연평균_미세먼지, 인구십만명당_문화기반시설수,  인구십만명당_사회복지시설수,  자살율,  사고건수_어, 사고건수_노)
columns_to_normalize <- c(
  "교원1인당_학생수", "학급당_유치원", "학급당_초등학교", "학급당_고등학교", "학급당_중학교",
  "어린이집수", "도서관수", "고등학교수", "중학교수", "초등학교수", "특수학교수", "외국인_국제학교",
  "일반요양병원수", "노인병원수", "일반병원수", "종합병원수", "한방병원수", "치과병원수", "부속의료기관수",
  "치과의원수", "한의원수", "일반의원수", "입원실수", "의료인수", "병상수", "경찰서", "소방서",
  "연평균_초미세먼지", "연평균_미세먼지", "인구십만명당_문화기반시설수", "인구십만명당_사회복지시설수",
  "자살율", "사고건수_어", "사고건수_노"
)
normalized_2019 <- normalized_2019 %>% mutate(across(all_of(columns_to_normalize) , ~ (.-min(.))/(max(.)-min(.))))

score_2019 <- normalized_2019 %>% summarise(code = code, 시군구 = 시군구.x,
                                            교육 = 교원1인당_학생수 * 0.5 + 어린이집수 * 3 + 도서관수 * 1 + 고등학교수 * 1 + 중학교수 * 1 +
                                              초등학교수 * 1 + 특수학교수 * 1 + 외국인_국제학교 * 1,
                                            위험도 = 경찰서 * 1 + 소방서 * 0.1 - 자살율 * 1 - 사고건수_어 * 1 - 사고건수_노 * 1,
                                            문화 = 인구십만명당_문화기반시설수 * 0.5 + 인구십만명당_사회복지시설수 * 0.5,
                                            건강 = 일반요양병원수 * 0.25 + 노인병원수 * 0.5 + 일반병원수 * 3 + 종합병원수 *3+한방병원수 * 1.5+
                                            치과병원수 * 0.25+부속의료기관수 * 1+치과의원수 * 0.125+한의원수 * 0.75+일반의원수 * 1.5+입원실수 * 0.5+의료인수 * 0.75+
                                            병상수 * 0.75+연평균_초미세먼지 * -0.5+연평균_미세먼지 * -0.5
                                            )
score_2019 <- score_2019 %>% mutate(across(all_of(c("교육", "위험도", "건강" , "문화")) , ~ (.-min(.))/(max(.)-min(.)))*99 + 1) 
score_2019 <- score_2019 %>% mutate(종합 = 교육+ 위험도+ 건강+문화)

# 2023년 데이터 취합 
data_2023 <- right_join(code, 교원_학생수2023, by = "code")
data_2023 <- left_join(data_2023, 학생수2023, by = "code")
data_2023 <- left_join(data_2023, all_kinder2023, by = "code")
data_2023 <- left_join(data_2023, 고등학교, by = "code")
data_2023 <- left_join(data_2023, 중학교, by = "code")
data_2023 <- left_join(data_2023, 초등학교, by = "code")
data_2023 <- left_join(data_2023, 특수학교, by = "code")
data_2023 <- left_join(data_2023, 외국인_국제학교, by = "code")
data_2023 <- left_join(data_2023, library, by = "code")
data_2023 <- left_join(data_2023, 요양병원_일반요양병원수_2023, by = "code")
data_2023 <- left_join(data_2023, 요양병원_노인병원수_2023, by = "code")
data_2023 <- left_join(data_2023, 일반병원수_2023, by = "code")
data_2023 <- left_join(data_2023, 종합병원수_2023, by = "code")
data_2023 <- left_join(data_2023, 한방병원수_2023, by = "code")
data_2023 <- left_join(data_2023, 치과병원수_2023, by = "code")
data_2023 <- left_join(data_2023, 병원_도구수_2023, by = "code")
data_2023 <- left_join(data_2023, 부속의료기관수_2023, by = "code")
data_2023 <- left_join(data_2023, 부속의료기관_도구수_2023, by = "code")
data_2023 <- left_join(data_2023, 치과의원수, by = "code")
data_2023 <- left_join(data_2023, 한의원수, by = "code")
data_2023 <- left_join(data_2023, 일반의원수, by = "code")
data_2023 <- left_join(data_2023, 의원_도구수, by = "code")
data_2023 <- left_join(data_2023, 경찰서, by = "code")
data_2023 <- left_join(data_2023, 소방서, by = "code")
data_2023 <- left_join(data_2023, 초미세먼지_2023, by = "code")
data_2023 <- left_join(data_2023, 미세먼지_2023, by = "code")
data_2023 <- left_join(data_2023, 문화기반시설2023, by = "code")
data_2023 <- left_join(data_2023, 사회복지시설2023, by = "code")
data_2023 <- left_join(data_2023, 어린이_교통사고2023, by = "code")
data_2023 <- left_join(data_2023, 노인_교통사고2023, by = "code")
data_2023 <- left_join(data_2023, 자살2023, by = "code")
data_2023 <- left_join(data_2023, population_2023, by = "code")
data_2023 <- left_join(data_2023, born2023, by = "code")

data_2023[is.na(data_2023)] <- 0
data_2023 <- data_2023 %>% mutate(의료인수 = 의료인수 + 의료인수.x+ 의료인수.y)
data_2023 <- data_2023 %>% mutate(입원실수 = 입원실수 + 입원실수.x + 입원실수.y)
data_2023 <- data_2023 %>% mutate(의료인수 = 병상수 + 병상수.x + 병상수.y)
data_2023 <- select(data_2023, -의료인수.x, -의료인수.y,-입원실수.x,-입원실수.y,-병상수.x,-병상수.y) 
names(data_2023)

#2023년 데이터 다중회귀로 확인
model <- lm(총인구수 ~ 교원1인당_학생수 + 학급당_유치원 + 학급당_초등학교 + 학급당_고등학교 +어린이집수+ 도서관수 + 고등학교수+
              중학교수 + 초등학교수 + 특수학교수+외국인_국제학교 +일반요양병원수+ 노인병원수+일반병원수
            +종합병원수+한방병원수+치과병원수+부속의료기관수+치과의원수+한의원수+일반의원수+입원실수 + 의료인수+병상수+경찰서+소방서+연평균_초미세먼지+연평균_미세먼지
            +인구십만명당_문화기반시설수 + 인구십만명당_사회복지시설수 + 자살율 + 사고건수_어 + 사고건수_노,  data = data_2023)
summary(model)

model <- lm(zero ~ 교원1인당_학생수 + 학급당_유치원 + 학급당_초등학교 + 학급당_고등학교 +어린이집수+ 도서관수 + 고등학교수+
              중학교수 + 초등학교수 + 특수학교수+외국인_국제학교 +일반요양병원수+ 노인병원수+일반병원수
            + 종합병원수+한방병원수+치과병원수+부속의료기관수+치과의원수+한의원수+일반의원수+입원실수+의료인수+병상수+경찰서+소방서+연평균_초미세먼지+연평균_미세먼지
            +인구십만명당_문화기반시설수 + 인구십만명당_사회복지시설수 + 자살율 + 사고건수_어 + 사고건수_노,data = data_2023)
summary(model)

# 2023년 데이터 정규화 (min-max 정규화 활용)
normalized_2023 <- select(data_2023, code, 시군구.x, 교원1인당_학생수, 학급당_유치원, 학급당_초등학교, 학급당_중학교, 학급당_고등학교, 어린이집수,도서관수,
                          고등학교수,
                          중학교수 , 초등학교수 , 특수학교수,외국인_국제학교 ,일반요양병원수, 노인병원수, 일반병원수
                          ,종합병원수, 한방병원수, 치과병원수,부속의료기관수, 치과의원수, 한의원수, 일반의원수, 입원실수, 의료인수, 병상수, 경찰서, 소방서, 연평균_초미세먼지, 
                          ,연평균_미세먼지, 인구십만명당_문화기반시설수,  인구십만명당_사회복지시설수,  자살율,  사고건수_어, 사고건수_노)

normalized_2023 <- normalized_2023 %>% mutate(across(all_of(columns_to_normalize) , ~ (.-min(.))/(max(.)-min(.))))

score_2023 <- normalized_2023 %>% summarise(code = code, 시군구 = 시군구.x,
                                            교육 = 교원1인당_학생수 * 0.5 + 어린이집수 * 3 + 도서관수 * 1 + 고등학교수 * 1 + 중학교수 * 1 +
                                              초등학교수 * 1 + 특수학교수 * 1 + 외국인_국제학교 * 1,
                                            위험도 = 경찰서 * 1 + 소방서 * 0.1 - 자살율 * 1 - 사고건수_어 * 1 - 사고건수_노 * 1,
                                            문화 = 인구십만명당_문화기반시설수 * 0.5 + 인구십만명당_사회복지시설수 * 0.5,
                                            건강 = 일반요양병원수 * 0.25 + 노인병원수 * 0.5 + 일반병원수 * 3 + 종합병원수 *3+한방병원수 * 1.5+
                                              치과병원수 * 0.25+부속의료기관수 * 1+치과의원수 * 0.125+한의원수 * 0.75+일반의원수 * 1.5+입원실수 * 0.5+의료인수 * 0.75+
                                              병상수 * 0.75+연평균_초미세먼지 * -0.5+연평균_미세먼지 * -0.5
)

score_2023 <- score_2023 %>% mutate(across(all_of(c("교육", "위험도", "건강" , "문화")) , ~ (.-min(.))/(max(.)-min(.)))*99 + 1) 
score_2023 <- score_2023 %>% mutate(종합 = 교육+ 위험도+ 건강+문화)