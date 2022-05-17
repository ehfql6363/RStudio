install.packages("stats")
install.packages("NbClust")
install.packages("cluster")
install.packages("factoextra")
library(stats)
library(NbClust)
library(cluster)
library(dplyr)
library(factoextra)

setwd("C:/Users/ehfql/Desktop/대학교/4학년-1/빅데이터 처리/강의 자료/10주차")
teens <- read.csv("snsdata.csv")
str(teens)

table(teens$gender)
summary(teens)

#데이터 전처리
teens$age <- ifelse(teens$age >= 13 & teens$age <20, teens$age, NA)
summary(teens$age)

#더미 변수
#클러스터링은 거리를 계산하기 때문에 수치로 바꿔줘야함.
teens$female <- ifelse(teens$gender == 'F' & !is.na(teens$gender), 1, 0)
#성별의 NA값도 하나의 변수로 보고 NA인 것을 1로 봄
teens$noGender <- ifelse(is.na(teens$gender), 1, 0)
#noGender가 0이고 female도 0이면 male

#결측치 대체 : 수치형 변수
mean(teens$age)
mean(teens$age, na.rm = TRUE)
#gradyear별로 age평균 구하기
teens %>% group_by(gradyear) %>% summarise(age_mean = mean(age, na.rm = TRUE))

#age가 NA인 곳에 년도별 age의 평균 값 넣기
#년도별 Age선언
aveAge <- ave(teens$age, teens$gradyear, FUN = function(x) mean(x, na.rm = TRUE))
#넣기
teens$age <- ifelse(is.na(teens$age), aveAge, teens$age)

summary(teens$age)

#모델링 데이터 셋 생성
data <- teens[,c(5:40)]
#사용할 변수들 표준화
interest <- as.data.frame(lapply(data, scale))#행령 형태를 as.data.frame()로 데이터 프레임 형태로 바꿈
#표준화를 위해 scale()사용
#data셋의 모든 변수들을 표준화 함수에 적용하기 위해 lapply()함수 이용

#모델링
#클러스터 찾기
set.seed(2345)
teen_cluster <- kmeans(interest, 5)
mycluster

#클러스터 검토
teen_cluster$cluster #클러스터 ID
tcluster <- teen_cluster$cluster
teen_cluster$centers #각 특징과 클러스터별 평균값 행렬
teen_cluster$size #각 클러스터에 할당된 데이터

#teens 데이터 셋에 클러스터 ID 추가
teens$cluster_id <- teen_cluster$cluster

teens %>% group_by(cluster_id) %>%
  summarise(age_mean = mean(age),
            female_man = mean(female),
            friend_mean = mean(friends))


#통계적 기법으로 클러스터 K구하는 방법
install.packages("factoextra")
library(factoextra)#클러스터 개수 구하기
library(rpart)
data("iris")

iris <- as.data.frame(iris)
iris1 <- -iris[,c(1:4)]

#elbo test
#동질성 기준(wss)
fviz_nbclust(iris1, kmeans, method = "wss")
nc <- NbClust(iris1, min.nc = 2, max.nc = 15, method = "kmeans")
barplot(table(nc$Best.n[1,]))
















