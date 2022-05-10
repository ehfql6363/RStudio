install.packages("arules") #선험적 (Apriori) 알고리즘
install.packages("arulesViz") # 연관규칙을 시각화

library(arules)
library(arulesViz)
library(dplyr)
library(tidyverse)

#getwd()
setwd("C:/Users/ehfql/Desktop/대학교/4학년-1/빅데이터 처리/강의 자료/9주차")
#read.csv("데이터 경로", header = FALSE)
a <- read.csv("groceries.csv", header = FALSE)
View(a)
#remove(a)
#희소행렬 sparse matrix : 행렬의 값이 대부분 0 / Null인 경우

#이진형으로 바꿔주기
groceries <- read.transactions("groceries.csv", sep=",")
#sep = ", " - > 쉼표로 분리됨
summary(groceries)

inspect(groceries[1:5]) #거래 파악, 처음 다섯 건의 거래 유형을 볼 수 있음

itemFrequency(groceries[,1:3]) #아이템을 포함한느 거래의 비율, 첫 번째 3개의 item비율을 볼 수 있음
#희소 행렬에서의 item은 알파벳 순으로 정렬

itemFrequencyPlot(groceries, support = 0.1) #item지지도의 시각화
# 최소 지지도가 0.1이상인 것
itemFrequencyPlot(groceries, topN = 20) #특정 개수만큼만 시각화, topN = 상위 몇개

#myrules <- apriori(data = groceries, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))
#0.006 = 하루에 2번 이상 구매한 경우를 1달로 계산 (한달에 60번이상인 것)
myrules <- apriori(data = groceries, parameter = list(support = 0.01, confidence = 0.25, minlen = 2))
#지지도가 0.01과 0.006인것 과의 lift의 차이가 생김

#data = 거래데이터를 가지고 있는 희소행렬
#support = 요구되는 최소 지지도 60 / 9835 = 0.006
#confidence = 요구되는 최소 신뢰도
#요구되는 최소 아이템 최소 규칙

inspect(myrules)
inspect(myrules[1:3])
inspect(sort(myrules, by="lift")[1:10])

#berry 관련 연관듀칙만 보기
berryrule <- subset(myrules, items %in% "berries")
inspect(berryrule)

#R데이터 프레임으로 규칙 저장
groceryrule <- as(myrules, "data.frame")
write.csv(groceryrule, file = "groceryrules.csv")

#그래프로 표현
plot(myrules, method="graph")
#표현방식 바꾸기
library(igraph)
plot(myrules, method="graph", engine="igraph")
#향상도(Lift)값이 높은 20개의 규칙만 그래프로 표현
plot(sort(myrules, by="lift")[1:20], method="graph", engine="igraph")
#특정 아이템을 선택하거나 룰을 선택하여 표현
plot(myrules, method="graph", engine="htmlwidget")
plot(sort(myrules, by="lift")[1:20], method="graph", engine="htmlwidget")

plot(myrules, method="grouped", engine="grid")
plot(myrules, method="grouped", interactive=TRUE)

#메트릭스 형태
#plot(myrules, method="grouped matrix") => 전체
plot(myrules, method="grouped matrix")
plot(sort(myrules, by="lift")[1:20], method="grouped matrix")









