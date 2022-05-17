library(readxl)
library(dplyr)
library(psych)
library(descr)
library(rpart)
library(adabag)
library(randomForest)
library(caret)
library(ggplot2)
library(party)
library(rpart.plot)


setwd("C:/Users/ehfql/Desktop/대학교/4학년-1/빅데이터 처리/강의 자료/6주차")
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

#genres_desc 범주화
box_office$genres <- as.factor(box_office$genres_desc)
table(box_office$genres)
#genres에는 소수의 장르가 있기 때문에 데이터 분석시
#속도에 영향을 줄 수 있음 => Other라는 값으로 변경
# Adventure, Horror, Crime은 빈도수가 비슷하여 AHC라는 이름으로 묶음
box_office$genres <- ifelse(box_office$genres == "Action",  "Action",
                            ifelse(box_office$genres == "Drama", "Drama",
                                   ifelse(box_office$genres == "Comedy", "Comedy", 
                                          ifelse(box_office$genres == "Adventure", "AHC", 
                                                 ifelse(box_office$genres == "Horror", "AHC",
                                                        ifelse(box_office$genres == "Crime", "AHC", "Others"))))))
#다시 범주화
box_office$genres <- as.factor(box_office$genres)

options(scipen = 100)

#Revenue 변수 범주화
describe(box_office$revenue)
summary(box_office$revenue)
#Revenue의 중위값을 기준
box_office$revenue <- ifelse(box_office$revenue >= 17514663, "above Med", "below Med")
box_office$revenue <- as.factor(box_office$revenue)
box_office$revenue
freq(box_office$revenue)

summary(box_office)
#분석에 불필요한 변수(col)제거
box_office2 <- box_office[, -c(2,3,4,6,7,9,10,11,13,14,15,16,17)]# <-최종 데이터
box_office <- box_office2[,-c(2,5)]
box_office_noGenre <- box_office[,-c(5)]

set.seed(111)
options(scipen = 100)

#createDataPartition 8:2 (revenue)
box_idx_rev <- createDataPartition(box_office$revenue, p = 0.8, list=FALSE)
train_rev <- box_office[box_idx_rev, ]
test_rev <- box_office[-box_idx_rev, ]
#noGenre
noBox_idx_rev <- createDataPartition(box_office_noGenre$revenue, p = 0.8, list=FALSE)
noTrain_rev <- box_office[noBox_idx_rev, ]
noTest_rev <- box_office[-noBox_idx_rev, ]
#
box2_idx_rev <- createDataPartition(box_office2$revenue, p=0.8, list = FALSE)
train2_rev <- box_office[box2_idx_rev, ]
test2_rev <- box_office[-box2_idx_rev, ]



# #createDataPartition 8:2 (popularity1)
# box_idx_pop <- createDataPartition(box_office$popularity, p=0.75, list = FALSE)
# train_pop <- box_office[box_idx_pop, ]
# 
# #createDataPartition 8:2 (genres)
# box_idx_gen <- createDataPartition(box_office$genres, p = 0.8, list = FALSE)
# train_gen <- box_office[box_idx_gen, ]

#train : test = 8 : 2 (smaple())
idx <- sample(2, nrow(box_office), replace = TRUE, prob=c(0.8, 0.2))

trainBoxOffice <- box_office[idx==1,]
table(trainBoxOffice$revenue)
table(trainBoxOffice$popularity)

testBoxOffice <- box_office[idx==2,]
table(testBoxOffice$revenue)
table(testBoxOffice$popularity)

#모델링
#1. rpart() - revenue
result_rpart_rev <- rpart(revenue ~., data = train_rev, control = rpart.control(minsplit = 2))
result_rpart_rev
rpart.plot(result_rpart_rev)
#noGenre
result_no_rpart_rev <- rpart(revenue ~., data = noTrain_rev, control = rpart.control(minsplit = 2))
result_no_rpart_rev
rpart.plot(result_no_rpart_rev)

#1-1.가지치기 prune()
result_rpart_rev$cptable

# #1. rpart() - popularity
# result_rpart_pop <- rpart(popularity ~., data = train_pop, control = rpart.control(minsplit = 2))
# result_rpart_pop
# rpart.plot(result_rpart_pop)
# 
# #1. raprt() - genres
# result_rpart_gen <- rpart(genres ~., data = train_gen, control = rpart.control(minsplit = 4))
# result_rpart_gen
# rpart.plot(result_rpart_gen)

#2. ctree() - revenue
result_ctree_rev <- ctree(revenue ~., data = train_rev, control = ctree_control(minsplit = 2))
result_ctree_rev
plot(result_ctree_rev)
#noGenre
result_no_ctree_rev <- ctree(revenue ~., data = noTrain_rev, control = ctree_control(minsplit = 2))
result_no_ctree_rev
plot(result_no_ctree_rev)

#3. randomForest()
result_rf_rev <- randomForest(revenue ~., data = train_rev, ntree = 100)
result_rf_rev
plot(result_rf_rev)
legend("topright", colnames(result_rf_rev$err.rate), cex = 0.8, fill = 1:3)
#noGenre
result_no_rf_rev <- randomForest(revenue ~., data = noTrain_rev, ntree = 100)
result_no_rf_rev
plot(result_no_rf_rev)
legend("topright", colnames(result_no_rf_rev$err.rate), cex = 0.8, fill = 1:3)

#4. Bagging() - 굉장히 오래 걸림...
result_bagging_rev2 <- bagging(revenue ~., data = train2_rev, mfinal = 10)
importance(result_bagging_rev)
#bagging 도식화
plot(result_bagging_rev$tree[[5]])
text(result_bagging_rev$tree[[5]])

#5. Boosting() - 이것도 오래 걸림...
result_boosting_rev2 <- boosting(revenue ~., data = train2_rev, mfinal = 1)


#변수의 중요도
barchart(importance(result_rf_rev))
varImpPlot(result_rf_rev)

#모델 평가 - rpart()
box_expect_rpart <- predict(result_rpart_rev, test_rev, type = "class")
box_expect_rpart
box_actual <- test_rev$revenue
#혼동행렬 사용
confusionMatrix(box_expect_rpart, box_actual, mode = "everything")

#모델 평가 - ctree()
box_expect_ctree <- predict(result_ctree_rev, test_rev, type = "response")
box_expect_ctree
confusionMatrix(box_expect_ctree, box_actual, mode = "everything")

#

#모델 평가 - randomForest()
box_expect_rf <- predict(result_rf_rev, test_rev)
confusionMatrix(box_expect_rf, box_actual, mode = "everything")


#그래프 기능ggplot

memory.size()
memory.limit(5000)



