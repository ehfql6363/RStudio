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

myrules <- apriori(data = groceries, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))
#data = 거래데이터를 가지고 있는 희소행렬
#support = 요구되는 최소 지지도 60 / 9835 = 0.006
#confidence = 요구되는 최소 신뢰도
#요구되는 최소 아이템 최소 규칙

inspect(myrules)
inspect(myrules[1:3])
inspect(sort(myrules, by="lift")[1:10])




















