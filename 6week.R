install.packages("adabag")
library(adabag)

data("iris")
View(iris)
str(iris)

iris_row_idx <- createDataPartition(iris$Species, p=0.8, list = FALSE)
iris_train <- iris[iris_row_idx,]
iris_test <- iris[-iris_row_idx,]

#bagging 알고리즘 bagging(formula, data=train-data, mfinal=number) mfinal= 반복 수 또는 트리의 수
iris.bagging <- bagging(Species~., data = iris, mifnal=10)

#분류 시 변수별 중요도, 모델 결과$importance
iris.bagging$importance
#도식화
barplot(iris.bagging$importance)
plot(iris.bagging$trees[[10]])
text(iris.bagging$trees[[10]])

expect2 <- predict(iris.bagging, iris_test, type="reponse")
#->Species를 예측하여 분류한 변수를 팩터화
expect2$class <- as.factor(expect2$class)
confusionMatrix(expect2$class, iris_test$Species, mode="everything")

#boosting bosting(formula, data=train_data, mfinal=number)
iris.ada <- boosting(Species~., data = iris_train, mfinal = 10)
iris.ada$importance
barchart(iris.ada$importance)

#도식화
plot(iris.ada$trees[[10]])
text(iris.ada$trees[[10]])

#예측 및 confusionMatrix
expect3 <- predict(iris.ada, iris_test, type="reponse")
expect3
expect3$class <- as.factor(expect3$class)
confusionMatrix(expect3$class, iris_test$Species, mode="everything")

#RandomForest
data(stagec)
View(stagec)

#RandomForest는 NA를 처리 해야함.
colSums(is.na(stagec))

#NA행 제거
stagec1 <- na.omit(stagec)

#RandomSampling 활용
set.seed(1234)
ind <- sample(2, nrow(stagec1), replace=TRUE, prob=c(0.7, 0.3))
trainData <- stagec1[ind==1,]
testData <- stagec1[ind==2,]


install.packages("randomForest")
library(randomForest)

#알고리즘 적용 randomForest(Formula~., data=trainData, ntree=number)
#ntree : tree의 생성 개수
rf <- randomForest(ploidy ~., data=trainData, ntree=10)

#변수의 중요도
importance(rf)
varImpPlot(rf)

#오분률
plot(rf)
legend("topright", colnames(rf$err.rate), cex=0.8, fill=1:4)#범례 정리리
rf
table(trainData$ploidy)

#모형평가하기
library(caret)
predict <- predict(rf, testData)
predict
testData$ploidy
confusionMatrix(predict, testData$ploidy, mode="everything")
