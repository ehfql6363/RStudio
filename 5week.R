install.packages("rpart")
library(rpart)
install.packages("party")
library(party)
install.packages("caret")
library(caret)
install.packages("rpart.plot")
library(rpart.plot)

data("iris")
View(iris)
str(iris)

iris_row_idx <- createDataPartition(iris$Species, p=0.8, list = FALSE) 
#iris데이터의 Species중 80%를 훈련용. 추출한 정보를 factor로
iris_train <- iris[iris_row_idx,]
str(iris_train)

# iris_train 데이터 셋의 꽃 종류별 데이터 수 확인(table 사용)
table(iris_train$Species)

# test 데이터 생성
iris_test <- iris[-iris_row_idx,]
table(iris_test$Species)

summary(iris_train)
summary(iris_test)

# 의사결정 나무 생성 rpart()
iris_result <- rpart(Species ~., data = iris_train, control = rpart.control(minsplit = 2))
iris_result

par(mfrow = c(1, 2))

# 의사결정 나무 도식화
rpart.plot(iris_result)

# 가지치기
iris_result$cptable
plotcp(iris_result)

iris_prune <- prune(iris_result, cp=0.025) #cp를 기준으로 가지치기
rpart.plot(iris_prune)
