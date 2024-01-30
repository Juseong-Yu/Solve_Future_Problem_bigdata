#라이브러리 정리
install.packages("devtools")
devtools::install_github("cardiomoon/kormaps2014")
install.packages("sf")
install.packages("rmapshaper")
install.packages("gridExtra")
library(kormaps2014)
library(sf)
library(rmapshaper)
library(ggplot2)
library(dplyr)

code <- read_excel("codecode.xlsx")

# 시군구 코드 정리
code <- distinct(code)
code$시도 <- sub("강원도", "강원특별자치도", code$시도)
#code <- code %>% mutate(code = paste(시도,시군구, sep = ""))
code <- code %>% mutate(시군구 = paste(시도,시군구, sep = " "))
code <- rename(code, code = 법정동코드)
code <- code %>% mutate(시군구 = ifelse(시군구 == "세종특별자치시 NA" , "세종특별자치시",시군구))
code <- code %>% select(시군구, code)

# 인구 지도 만들기
legend_limits = c(-25, 25)

population_c <- left_join(population_2022, population_2023, by = "code")
population_c <- left_join(population_c, code, by = "code")
population_c <- population_c %>% mutate(증감율 = (총인구수.y-총인구수.x)/총인구수.x * 100)

pop_map <- read_sf("sig.shp")
pop_map = pop_map |> st_transform(5179)
pop_map = pop_map |> 
  mutate(SIG_KOR_NM = iconv(SIG_KOR_NM, from = "EUC-KR", to = 'UTF-8'))
pop_map = pop_map |> 
  ms_simplify(keep = 0.01, keep_shapes = T)
pop_map <- rename(pop_map, code= SIG_CD)
pop_map$code <- as.numeric(pop_map$code)
pop_map <- left_join(pop_map, population_c , by = "code")
ggplot(pop_map) +
  geom_sf(aes(fill = 증감율), color = "black") +
  scale_fill_gradientn(colors = c("darkblue", "blue", "white","red", "darkred"), guide = "legend", limits = legend_limits) +
  theme_minimal() +
  theme(legend.title = element_blank())



# 0세 지도 만들기 
legend_limits = c(-50, 50)

born_c <- left_join(born2021, born2022, by = "code")
born_c <- born_c %>% mutate(증감율 = (zero.y-zero.x)/zero.x * 100)

pop_map <- read_sf("sig.shp")
pop_map = pop_map |> st_transform(5179)
pop_map = pop_map |> 
  mutate(SIG_KOR_NM = iconv(SIG_KOR_NM, from = "EUC-KR", to = 'UTF-8'))
pop_map = pop_map |> 
  ms_simplify(keep = 0.01, keep_shapes = T)
pop_map <- rename(pop_map, code= SIG_CD)
pop_map$code <- as.numeric(pop_map$code)
pop_map <- left_join(pop_map, born_c , by = "code")
ggplot(pop_map) +
  geom_sf(aes(fill = 증감율), color = "black") +
  scale_fill_gradientn(colors = c("darkred", "white", "darkblue"), guide = "legend", limits = legend_limits) +
  theme_minimal() +
  theme(legend.title = element_blank())

# 2019년 각 변수별 최하위도시
theme_set(theme_grey(base_family='NanumGothic'))

교육_2019 <- score_2019 %>% select(시군구, 교육)
위험도_2019 <- score_2019 %>% select(시군구, 위험도)
건강_2019 <- score_2019 %>% select(시군구, 건강)
문화_2019 <- score_2019 %>% select(시군구, 문화)
종합_2019 <- score_2019 %>% select(시군구, 종합)


library(gridExtra)
box2019_교육 <- ggplot(data = score_2019 , aes(x = 1, y = 교육)) + geom_boxplot()
box2019_위험도 <- ggplot(data = score_2019 , aes(x = 1, y = 위험도)) + geom_boxplot()
box2019_건강 <- ggplot(data = score_2019 , aes(x = 1, y = 건강)) + geom_boxplot()
box2019_문화 <- ggplot(data = score_2019 , aes(x = 1, y = 문화)) + geom_boxplot()
box2019_종합 <- ggplot(data = score_2019 , aes(x = 1, y = 종합)) + geom_boxplot()

grid.arrange(box2019_교육, box2019_위험도, box2019_건강,box2019_문화, box2019_종합,ncol = 5)

교육_2019 <- 교육_2019 %>% arrange(교육)
위험도_2019 <- 위험도_2019 %>% arrange(위험도)
건강_2019 <- 건강_2019 %>% arrange(건강)
문화_2019 <- 문화_2019 %>% arrange(문화)
종합_2019 <- 종합_2019 %>% arrange(종합)

교육_2019 <- head(교육_2019, 10)
위험도_2019 <- head(위험도_2019, 10)
건강_2019 <- head(건강_2019, 10)
문화_2019 <- head(문화_2019, 10)
종합_2019 <- head(종합_2019, 10)


ggplot(data = 교육_2019 , aes(x = reorder(시군구, 교육), y = 교육, fill = 교육)) + 
  geom_col()+ 
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+ 
  labs(title = "2019년 교육점수 최하위 10개 도시" , x = "도시명", y = "점수")+
  scale_fill_gradient(low = "lightblue", high = "darkblue")

ggplot(data = 위험도_2019 , aes(x = reorder(시군구, 위험도), y = 위험도, fill = 위험도)) + 
  geom_col()+ 
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+ 
  labs(title = "2019년 위험도점수 최하위 10개 도시" , x = "도시명", y = "점수")+
  scale_fill_gradient(low = "lightblue", high = "darkblue")

ggplot(data = 건강_2019 , aes(x = reorder(시군구, 건강), y = 건강,fill = 건강)) + 
  geom_col()+ 
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+ 
  labs(title = "2019년 건강점수 최하위 10개 도시" , x = "도시명", y = "점수")+
  scale_fill_gradient(low = "lightblue", high = "darkblue")

ggplot(data = 문화_2019 , aes(x = reorder(시군구, 문화), y = 문화,fill = 문화)) + 
  geom_col()+ 
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+ 
  labs(title = "2019년 문화점수 최하위 10개 도시" , x = "도시명", y = "점수")+
  scale_fill_gradient(low = "lightblue", high = "darkblue")

ggplot(data = 종합_2019 , aes(x = reorder(시군구, 종합), y = 종합,fill = 종합)) + 
  geom_col()+ 
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+ 
  labs(title = "2019년 종합점수 최하위 10개 도시" , x = "도시명", y = "점수")+
  scale_fill_gradient(low = "lightblue", high = "darkblue")

# 2023년 최하위 데이터 추출
교육_2023 <- score_2023 %>% select(시군구, 교육)
위험도_2023 <- score_2023 %>% select(시군구, 위험도)
건강_2023 <- score_2023 %>% select(시군구, 건강)
문화_2023 <- score_2023 %>% select(시군구, 문화)
종합_2023 <- score_2023 %>% select(시군구, 종합)

library(gridExtra)
box2023_교육 <- ggplot(data = score_2023 , aes(x = 1, y = 교육)) + geom_boxplot()
box2023_위험도 <- ggplot(data = score_2023 , aes(x = 1, y = 위험도)) + geom_boxplot()
box2023_건강 <- ggplot(data = score_2023 , aes(x = 1, y = 건강)) + geom_boxplot()
box2023_문화 <- ggplot(data = score_2023 , aes(x = 1, y = 문화)) + geom_boxplot()
box2023_종합 <- ggplot(data = score_2023 , aes(x = 1, y = 종합)) + geom_boxplot()

grid.arrange(box2023_교육, box2023_위험도, box2023_건강,box2023_문화, box2023_종합,ncol = 5)

교육_2023 <- 교육_2023 %>% arrange(교육)
위험도_2023 <- 위험도_2023 %>% arrange(위험도)
건강_2023 <- 건강_2023 %>% arrange(건강)
문화_2023 <- 문화_2023 %>% arrange(문화)
종합_2023 <- 종합_2023 %>% arrange(종합)

교육_2023 <- head(교육_2023, 10)
위험도_2023 <- head(위험도_2023, 10)
건강_2023 <- head(건강_2023, 10)
문화_2023 <- head(문화_2023, 10)
종합_2023 <- head(종합_2023, 10)


ggplot(data = 교육_2023 , aes(x = reorder(시군구, 교육), y = 교육, fill = 교육)) + 
  geom_col() + 
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) + 
  labs(title = "2023년 교육 최하위 10개 도시", x = "도시명", y = "점수") + 
  scale_fill_gradient(low = "lightblue", high = "darkblue")

ggplot(data = 위험도_2023 , aes(x = reorder(시군구, 위험도), y = 위험도, fill = 위험도)) + 
  geom_col()+ 
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+ 
  labs(title = "2023년 위험도 최하위 10개 도시" , x = "도시명", y = "점수")+
  scale_fill_gradient(low = "lightblue", high = "darkblue")

ggplot(data = 건강_2023 , aes(x = reorder(시군구, 건강), y = 건강, fill = 건강)) + 
  geom_col()+ 
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+ 
  labs(title = "2023년 건강 최하위 10개 도시" , x = "도시명", y = "점수")+
  scale_fill_gradient(low = "lightblue", high = "darkblue")

ggplot(data = 문화_2023 , aes(x = reorder(시군구, 문화), y = 문화, fill = 문화)) + 
  geom_col()+ 
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+ 
  labs(title = "2023년 문화 최하위 10개 도시" , x = "도시명", y = "점수")+
  scale_fill_gradient(low = "lightblue", high = "darkblue")

ggplot(data = 종합_2023 , aes(x = reorder(시군구, 종합), y = 종합, fill = 종합)) + 
  geom_col()+ 
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+ 
  labs(title = "2023년 종합 최하위 10개 도시" , x = "도시명", y = "점수")+
  scale_fill_gradient(low = "lightblue", high = "darkblue")

#종합점수 지도데이터
legend_limits = c(50, 250)
pop_map <- read_sf("sig.shp")
pop_map = pop_map |> st_transform(5179)
pop_map = pop_map |> 
  mutate(SIG_KOR_NM = iconv(SIG_KOR_NM, from = "EUC-KR", to = 'UTF-8'))
pop_map = pop_map |> 
  ms_simplify(keep = 0.01, keep_shapes = T)
pop_map <- rename(pop_map, code= SIG_CD)
pop_map$code <- as.numeric(pop_map$code)
pop_map <- left_join(pop_map, score_2019 , by = "code")
ggplot(pop_map) +
  geom_sf(aes(fill = 종합), color = "black") +
  scale_fill_gradientn(colors = c("white","red"), guide = "legend", limits = legend_limits) +
  theme_minimal() +
  theme(legend.title = element_blank())

pop_map <- read_sf("sig.shp")
pop_map = pop_map |> st_transform(5179)
pop_map = pop_map |> 
  mutate(SIG_KOR_NM = iconv(SIG_KOR_NM, from = "EUC-KR", to = 'UTF-8'))
pop_map = pop_map |> 
  ms_simplify(keep = 0.01, keep_shapes = T)
pop_map <- rename(pop_map, code= SIG_CD)
pop_map$code <- as.numeric(pop_map$code)
pop_map <- left_join(pop_map, score_2023 , by = "code")
ggplot(pop_map) +
  geom_sf(aes(fill = 종합), color = "black") +
  scale_fill_gradientn(colors = c("white","red"), guide = "legend", limits = legend_limits) +
  theme_minimal() +
  theme(legend.title = element_blank())

# 2019년과 2023년 점수 비교
overall_score <- inner_join(score_2019, score_2023 , by = "code")
overall_score <- overall_score %>% 
  mutate(교육변동 = (교육.y -교육.x)/교육.x * 100 ,문화변동 = (문화.y -문화.x) / 문화.x  * 100,위험도변동 = (위험도.y -위험도.x) / 위험도.x * 100,건강변동 = (건강.y -건강.x)/ 건강.x * 100,종합변동 = (종합.y - 종합.x) / 종합.x * 100 )

overall_score <- overall_score %>% select(시군구.x, code,교육변동,문화변동,위험도변동,건강변동,종합변동)

pop_map <- read_sf("sig.shp")
pop_map = pop_map |> st_transform(5179)
pop_map = pop_map |> 
  mutate(SIG_KOR_NM = iconv(SIG_KOR_NM, from = "EUC-KR", to = 'UTF-8'))
pop_map = pop_map |> 
  ms_simplify(keep = 0.01, keep_shapes = T)
pop_map <- rename(pop_map, code= SIG_CD)
pop_map$code <- as.numeric(pop_map$code)
pop_map <- left_join(pop_map, overall_score , by = "code")
ggplot(pop_map) +
  geom_sf(aes(fill = 종합변동), color = "black") +
  scale_fill_gradientn(colors = c("darkblue", "white", "darkred"), guide = "legend", limits = legend_limits) +
  theme_minimal() +
  theme(legend.title = element_blank())

# 인구감소지역 89곳
인구감소지역 = c("부산광역시 동구","부산광역시 서구","부산광역시 영도구",
           "대구광역시 남구","대구광역시 서구","경상북도 군위군",
           "인천광역시 강화군","인천광역시 옹진군",
           "경기도 가평군","경기도 연천군",
           "강원특별자치도 고성군","강원특별자치도 삼척시","강원특별자치도 양구군","강원특별자치도 양양군","강원특별자치도 영월군","강원특별자치도 정선군","강원특별자치도 철원군","강원특별자치도 태백시","강원특별자치도 평창군","강원특별자치도 홍천군","강원특별자치도 화천군","강원특별자치도 횡성군",
           "충청북도 괴산군","충청북도 단양군","충청북도 보은군","충청북도 영동군","충청북도 옥천군","충청북도 제천시",
           "충청남도 공주시","충청남도 금산군","충청남도 논산시","충청남도 보령시","충청남도 부여군","충청남도 서천군","충청남도 예산군","충청남도 청양군","충청남도 태안군",
           "전라북도 고창군","전라북도 김제시","전라북도 남원시","전라북도 무주군","전라북도 부안군","전라북도 순창군","전라북도 임실군","전라북도 장수군","전라북도 정읍시","전라북도 진안군",
           "전라남도 강진군","전라남도 고흥군","전라남도 곡성군","전라남도 구례군","전라남도 담양군","전라남도 보성군","전라남도 신안군","전라남도 영광군","전라남도 영암군","전라남도 완도군","전라남도 장성군","전라남도 장흥군","전라남도 진도군","전라남도 함평군","전라남도 해남군","전라남도 화순군",
           "경상북도 고령군","경상북도 문경시","경상북도 봉화군","경상북도 상주시","경상북도 성주군","경상북도 안동시","경상북도 영덕군","경상북도 영양군","경상북도 영주시","경상북도 영천시","경상북도 울릉군","경상북도 울진군","경상북도 의성군","경상북도 청도군","경상북도 청송군",
           "경상남도 거창군","경상남도 고성군","경상남도 남해군","경상남도 밀양시","경상남도 산청군","경상남도 의령군","경상남도 창녕군","경상남도 하동군","경상남도 함안군","경상남도 함양군","경상남도 합천군")
length(인구감소지역)
pop_dec <- left_join(population_2019, population_2023, by = "code")
pop_dec <- left_join(pop_dec, overall_score, by = "code")
pop_dec <- left_join(pop_dec, born2019, by = "code")
pop_dec <- left_join(pop_dec,  born2023, by = "code")
pop_dec <- left_join(pop_dec, code, by = "code")
pop_dec <- pop_dec %>% mutate(인구증감율 = (총인구수.y-총인구수.x)/총인구수.x * 100)
pop_dec <- pop_dec %>% mutate(영세증감율 = (zero.y-zero.x)/zero.x * 100)
pop_dec <- pop_dec %>% filter(시군구 %in% 인구감소지역 )

pop_dec_인구 <- pop_dec %>% arrange(인구증감율)
pop_dec_영세 <- pop_dec %>% arrange(영세증감율)
pop_dec_교육 <- pop_dec %>% arrange(교육변동)
pop_dec_문화 <- pop_dec %>% arrange(문화변동)
pop_dec_위험 <- pop_dec %>% arrange(위험도변동)
pop_dec_건강 <- pop_dec %>% arrange(건강변동)
pop_dec_종합 <- pop_dec %>% arrange(종합변동)

pop_dec_인구 <- rbind(head(pop_dec_인구),tail(pop_dec_인구))
pop_dec_영세 <- rbind(head(pop_dec_영세),tail(pop_dec_영세))
pop_dec_교육 <- rbind(head(pop_dec_교육),tail(pop_dec_교육))
pop_dec_문화 <- rbind(head(pop_dec_문화),tail(pop_dec_문화))
pop_dec_위험 <- rbind(head(pop_dec_위험),tail(pop_dec_위험))
pop_dec_건강 <- rbind(head(pop_dec_건강),tail(pop_dec_건강))
pop_dec_종합 <- rbind(head(pop_dec_종합),tail(pop_dec_종합))


ggplot(data = pop_dec_인구 , aes(x = reorder(시군구, 인구증감율), y = 인구증감율, fill = 인구증감율)) + 
  geom_col()+ 
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+ 
  labs(title = "인구감소지역중 인구 증가율 top5, bottom5" , x = "도시명", y = "증가율")+
  scale_fill_gradient(low = "lightblue", high = "darkblue")

ggplot(data = pop_dec_영세 , aes(x = reorder(시군구, 영세증감율), y = 영세증감율, fill = 영세증감율)) + 
  geom_col()+ 
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+ 
  labs(title = "인구감소지역중 영세 증가율 top5, bottom5" , x = "도시명", y = "증가율")+
  scale_fill_gradient(low = "lightblue", high = "darkblue")

ggplot(data = pop_dec_교육 , aes(x = reorder(시군구, 교육변동), y = 교육변동, fill = 교육변동)) + 
  geom_col()+ 
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+ 
  labs(title = "인구감소지역중 교육변동 top5, bottom5" , x = "도시명", y = "증가율")+
  scale_fill_gradient(low = "lightblue", high = "darkblue")

ggplot(data = pop_dec_문화 , aes(x = reorder(시군구, 문화변동), y = 문화변동, fill = 문화변동)) + 
  geom_col()+ 
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+ 
  labs(title = "인구감소지역중 문화변동 top5, bottom5" , x = "도시명", y = "증가율")+
  scale_fill_gradient(low = "lightblue", high = "darkblue")

ggplot(data = pop_dec_위험 , aes(x = reorder(시군구, 위험도변동), y = 위험도변동, fill = 위험도변동)) + 
  geom_col()+ 
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+ 
  labs(title = "인구감소지역중 위험도변동 top5, bottom5" , x = "도시명", y = "증가율")+
  scale_fill_gradient(low = "lightblue", high = "darkblue")

ggplot(data = pop_dec_건강 , aes(x = reorder(시군구, 건강변동), y = 건강변동, fill = 건강변동)) + 
  geom_col()+ 
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+ 
  labs(title = "인구감소지역중 건강변동 top5, bottom5" , x = "도시명", y = "증가율")+
  scale_fill_gradient(low = "lightblue", high = "darkblue")

ggplot(data = pop_dec_종합 , aes(x = reorder(시군구, 종합변동), y = 종합변동, fill = 종합변동)) + 
  geom_col()+ 
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))+ 
  labs(title = "인구감소지역중 종합변동 top5, bottom5" , x = "도시명", y = "증가율")+
  scale_fill_gradient(low = "lightblue", high = "darkblue")
