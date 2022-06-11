library(arules)
library(arulesViz)
library(dplyr)
library(tidyverse)

setwd("C:/Users/Admin/Desktop/대학교/2022-1/빅데이터 처리/과제/3")

market <- read.transactions("Market_Basket_Optimization.csv", sep=",")
summary(market)
#행렬의 크기 892,619, 0이 아닌 셀의 비율(Density) = 0.03288973
#따라서, 한달에 약 29,358개의 아이템이 중복구매를 포함하여 구매됨.
#이에, 거래 건수로 나누면 한달에 한번 거래하는데 구매하는 평균 아이템 개수가 나옴.
#29,358 / 7,501 = 약 3.913(개)

#처음 5건의 거래 보기
inspect(market[1:5])

#상위 20개의 물건 보기
itemFrequencyPlot(market, topN=20)
