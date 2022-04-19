#데이터 및 그래프 관련
install.packages('tidyverse')
install.packages('gapminder')
#색상
install.packages('nord')
install.packages('viridis')
#애니메이션
install.packages('gganimate')
install.packages('gifski')
install.packages('av')
#지도관련
install.packages('ggiraphExtra')
install.packages('maps')
install.packages('mapproj')

library(dplyr)
library(tidyverse)
library(gapminder)

library(nord)
library(viridis)

library(gganimate)
library(gifski)
library(av)

library(ggiraphExtra)
library(maps)
library(mapproj)

#데이터 프레임 구조화
data("gapminder")
gpaminder <- as.data.frame(gapminder)

gapminder
?gapminder

options(scipen = 999)

#1. ggplot에 정의한 내용 저장
p <- ggplot(data = gapminder)
#2. aes()를 이용하여 데이터의 어떠한 것들을 시각적으로 맵핑할 것인지
p <- ggplot(data = gapminder, mapping = aes(gdpPercap, y = lifeExp))

p
str(p)

#3. geom_함수를 이용하여 도표에 미학적 요소 추가
#산점도 geom_point()
p + geom_point()
#추세선 표현 및 표준오차
p + geom_smooth()
#둘 다
p + geom_point() + geom_smooth()

#4. 축의 척도 변환
#1인당 국민 총생산이 밀집 -> x축 스케일을 로그변환. scale_x_log10()이용
p + geom_point() + geom_smooth(method = "gam") + scale_x_log10()
#method = "gam"은 일반화 가법 모형(default)
#method = "lm"은 선형
#*geom_함수의 미적 요소 추가
#a. 산점도 색상 보라(color = "purple")
#b. 추세선은  선형 (method = "lm")
#c. 추세선 색상 오렌지 (color = "orange")
#d. 표준 오차 없애기 (se = FALSE)
#e. 추세선 두껍게 (size = 6)
#f. x축 스케일을 상용로그로 변환 (sclae_x_log10())
p + geom_point(color = "purple") + geom_smooth(method = "lm", color = "orange", se=FALSE, size = 6) + scale_x_log10()

#5. 레이블 추가
p + geom_point() + geom_smooth(method = "gam") + scale_x_log10() + labs(x = 'GDP Per Capita', y = 'Life Expectancy in Years', title = '경제성장률과 기대수명', subtitle = '데이터 포인트는 연도별', caprion = '자료:갭마인더')


#4개의 차원을 기술 (축으로는 2차원만 가능) 따라서, 그 이상은 색상이나 크기 등으로 표현
#크기는 규모를 나타낼 수 있는 변수 (연속형변수)
#색상은 구분을 하는 변수 (범주형 변수)
p1 <- ggplot(data = gapminder,
             mapping = aes(x = gdpPercap, y = lifeExp,
                           color = continent, size = pop, fill = continent))
#fill = continent -> se를 continent색으로 채우기
p1 + geom_point(alpha = 0.3) + geom_smooth() + scale_x_log10()
p1 + geom_point(alpha = 0.3) + geom_smooth() + scale_x_log10() + labs(x='GDP Per Capita', y = 'Life Expectancy in Years',
                                                                      title = '대륙별 경제 성장률과 기대수명',
                                                                      subtitle = '데이터 포인트는 연도별',
                                                                      caption = '자료 : 갭마인더더')

#그래프에 애니메이션 추가
ani1 <- p1 + geom_point(alpha=0.5) +
  scale_color_viridis(option = "C", discrete = TRUE) +#색상 #discrete : 개별 팔레트를 사용할 것인가?
  scale_x_log10()+
  theme_minimal()+#배경
  theme(legend.position = 'right')+ #범례위치
  #애니메이션 길이
  transition_states(year, state_length = 0.5) + #동작의 기준이 되는 변수 = year. year가 변할 때마다 걸리는 시간 = 0.5초
  #그래프 타이틀
  ggtitle('Now showing {closest_state}')

ani1

#분석 애니메이션 저장
animate(ani1, renderer = av_renderer())


#연도별 경제수준의 차이 : 시간에 따른 비교 -> 라인 차트
q <- ggplot(data = gapminder,
            mapping = aes(x = year, y = gdpPercap))
q + geom_line() #이건 각 년도마다 국가들을 찍어줌

q + geom_line(aes(group = country)) #국가별로 묶어서 국가마다 년도별 GDP를 보여줌


#facet 소규모 다중 도표를 만드는 패싯
#전체 프롯을 하나 또는 그 이상의 이산형 변수로 플롯을 나눔
q <- ggplot(data = gapminder,
            mapping = aes(x = year, y = gdpPercap)) + geom_line(aes(group = country)) + facet_wrap(~ continent)#대륙
q <- ggplot(data = gapminder,
            mapping = aes(x = year, y = gdpPercap, 
                          color = continent)) + geom_line(aes(group = country)) + facet_wrap(~ continent,
                                                                                                   ncol = 5)
q



#지도 그리기
#단계 구분도

#미국 주별 범죄 데이터 (4개의 범죄 종류, 50개 주 데이터)
data("USArrests")
USArrests <- data.frame(USArrests)
View(USArrests)
str(USArrests)

#행 이름을 컬럼 변수(state)로 변환, crime이라는 새로운 데이터 셋 생성(중요)
crime <- rownames_to_column(USArrests, var = "state")
str(crime)

#지도 데이터의 주 이름이 소문자 -> state컬럼 변수의 값을 소문자로 수정
crime$state <- tolower(crime$state)
head(crime)

#지도 데이터
state_map <- map_data("state")
View(state_map)

#단계 구분도 시각화
#ggChoropleth(data, aes(fill = , map_id = ), map = ,interactive  =)
#map_id = 지도로 시각화할 data와 지도 정보, 위치 정보가 있는 map데이터를 연결할 수 있는 변수 이름
#fill = 지도의 각 그룹을 색깔로 채울 변수
#interactive = 마우스오버 시, 데이터 표시 여부
ggChoropleth(data = crime, aes(fill = Murder, map_id=state), map = state_map, interactive = T)











