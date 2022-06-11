library(arules)
library(arulesViz)
library(dplyr)
library(tidyverse)


#연관성 분석

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

#의미있는 연관분석 룰 설정
marketRule <- apriori(data = market, parameter = list(support=0.0079, confidence=0.25, minlen=2))
#지지도(support)를 0.0079로 설정한 이유
#하나의 물건이 하루에 2번 이상 구매하면 의미있는 규칙이라고 판단
#즉, 60 / 7,501 = 약 0.0079

#confidence를 기준으로 TOP20 출력
inspect(sort(marketRule, by="confidence")[1:20])

#confidence를 기준으로 정렬한 이유
#구매하는 비중(support)이나 향상도를 기준으로 잡을 수 있지만,
#신뢰도(confidence)를 통해 연관되는 물품을 분석하고, 행사 등에 쓰일 물품 등을
#파악할 때 사용되며, 신로도로 판단하여 물품 사이의 연관규칙을 보다 정확하게 분석할 수 있다고 판단.

#활용
#좌변(LHS)의 물품을 산 사람이 우변(LHS)의 물품을 살 확률을 신뢰도(confidence)라고 말할 수 있다.
#즉, 신뢰도를 통해 묶음 판매 등의 비즈니스 마케팅으로 활용이 가능하며, 조합이 가능한 물품일 경우
#신뢰도가 좋은 물품끼리 합친 신제품 출시 등으로 활용이 가능하다.

#시각화
library(igraph)

plot(marketRule, method = "graph", engine = )
plot(sort(marketRule, by="confidence")[1:20], method="graph", engine="igraph")

plot(marketRule, method="grouped", engine="grid")
plot(marketRule, method="grouped", interactive=TRUE)



#군집 분석
library(stats)
library(NbClust)
library(cluster)
library(dplyr)
library(factoextra)
library(factoextra)#클러스터 개수 구하기
library(rpart)

USArrests <- data.frame(USArrests)
View(USArrests)
arrests <- as.data.frame(lapply(USArrests,scale))

#동질성 기준(wss)으로 클러스터의 수 결정
fviz_nbclust(USArrests, kmeans, method = "wss")
#클러스터의 수 2으로 설정

#정한 클러스터 수로 클러스터링 수행
set.seed(1234)
uaCluster <- kmeans(arrests, 2)
#검토
uaCluster$cluster
uaCluster$centers
uaCluster$size

USArrests$clusterID <- uaCluster$cluster

USArrests %>% group_by(clusterID) %>%
  summarise(Murder_mean = mean(Murder),
            Assault_man = mean(Assault),
            UrbanPop_mean = mean(UrbanPop),
            Rape_mean = mean(Rape))


#군집별 특성 정의 및 군집명 명명







