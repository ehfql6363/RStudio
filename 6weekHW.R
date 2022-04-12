library(readxl)
library(dplyr)
library(psych)
library(descr)
library(rpart)
library(adabag)
library(randomForest)
library(caret)
library(ggplot2)
install.packages("rpart.plot")
library(rpart.plot)


setwd("C:/Users/Admin/Desktop/대학교/2022-1/빅데이터 처리/강의자료/6주차")
box_office_full <- read_excel("train_box_office.xlsx")
head(box_office_full)
tail(box_office_full)
View(box_office_full)

#변수별 결측치 확인
colSums(is.na(box_office_full))

#NA가 많은 변수(col) 제외 및 행(row) 제외, 식별자 제외
box_office1 <- box_office_full[, -c(1,2,6,13,19,21)]
box_office <- na.omit(box_office1)
box_office

#사용하지 않을 변수 찾기
freq(box_office$status)
freq(box_office$original_language)

#text 구조화
box_office$production_countries1 <- substr(box_office$production_countries, 18, 19)
box_office$production_countries1

#Revenue 변수 범주화
describe(box_office$revenue)
#Revenue의 평균을 기준
box_office$revenue1 <- ifelse(box_office$revenue >= 67108787, "above AVG", "below AVG")
box_office$revenue1 <- as.factor(box_office$revenue1)
box_office$revenue1
freq(box_office$revenue1)

#Popularity 변수 범주화
describe(box_office$popularity)
#Popularity의 중앙값을 기준
box_office$popularity1 <- ifelse(box_office$popularity >= 7.48, "above med", "below med")
box_office$popularity1 <- as.factor(box_office$popularity1)
box_office$popularity1
freq(box_office$popularity1)
#기준을 중앙값으로 잡는 이유
#두 범주의 빈도수가 가장 비슷하기 때문

summary(box_office)
#분석에 불필요한 변수(col)제거
box_office <- box_office[, -c(2,4,6,7,9,10,11,13,14,15,17,18)]
box_office <- box_office[,-c(6)] # <-최종 데이터

set.seed(111)

#createDataPartition 8:2 (revenue1)
box_idx_rev <- createDataPartition(box_office$revenue1, p = 0.8, list=FALSE)
train_rev <- box_office[box_idx_rev, ]

#createDataPartition 8:2 (popularity1)
box_idx_pop <- createDataPartition(box_office$popularity1, p=0.75, list = FALSE)
train_pop <- box_office[box_idx_pop, ]

#train : test = 8 : 2 (smaple())
idx <- sample(2, nrow(box_office), replace = TRUE, prob=c(0.8, 0.2))
trainBoxOffice <- box_office[idx==1,]
testBoxOffice <- box_office[idx==2,]

#모델링
#1. rpart() - revenue1
result_rpart_rev <- rpart(revenue1 ~., data = train_rev, control = rpart.control(minsplit = 2))
result_rpart_rev
rpart.plot(result_rpart_rev)

#1. rpart() - popularity1
result_rpart_pop <- rpart(popularity1 ~., data = train_pop, control = rpart.control(minsplit = 2))
result_rpart_pop
rpart.plot(result_rpart_pop)




