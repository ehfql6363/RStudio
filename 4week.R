install.packages('readxl')
install.packages('Rcpp')
install.packages('psych')
install.packages('descr')
install.packages('dplyr')
library(readxl)
library(psych)

setwd("C:/Users/ehfql/Desktop/대학교/4학년-1/빅데이터 처리/강의 자료/4주차")
sample1 <- read_excel("sample1.xlsx")

summary(sample1)
describe(sample1[,c(3, 5:8)])

hist(sample1$AGE, #히스토 그램 기본
     xlim = c(0,60), #히스토그램 옵션 x의 범위 지정
     ylim = c(0,5), # y의 범위 지정
     main = "연령대 분포",  # 히스토그램 제목
     xlab = "연령대", # x축 이름
     ylab = "빈도수", # y축 이름
     col = "Yellow", # 색지정
     breaks = 8) # 구간개수

quantile(sample1$AMT17) # 사분위수
quantile(sample1$AMT16)

boxplot(sample1$AMT17, sample1$AMT16,
        ylim = c(0,1500000),
        main = "카드사용 Boxplot",
        names = c("2017년 카드사용금액", "2016년 카드사용금액"),
        col = c("Green", "Yellow"))

outlier <- factor(c(1, 2, 3, 4, 5, 6, 7, 8, 80, 30), levels = c(1, 2, 3, 4, 5, 6, 7, 8, 30, 80), ordered = "TRUE")
boxplot(outlier)

# 지역별 17년 16년 카드 사용건수, 사용금액
describeBy(sample1[,c(5:8)], group = sample1$AREA)

# 성별 17년 16년 카드 사용건수, 사용금액
describeBy(sample1[,c(5:8)], group = sample1$Gender)

# 윈도우 나누기
# par(mfrow = (행의 수, 열의 수))
par(mfrow = c(1, 1))

# 각 범주별로 히스토그램그리기
# hist(수치형변수[범주형번수 == "범주값])
hist(sample1$AMT17[sample1$Gender == "F"],
     main = "2017년 카드사용금액(여성)",
     xlab = "카드사용액",
     ylab = "빈도수",
     col = "Yellow")
hist(sample1$AMT17[sample1$Gender == "M"],
     main = "2017년 카드사용금액(남성)",
     xlab = "카드사용액",
     ylab = "빈도수",
     col = "Green")

options(scipen = 999)

#수치형 변수 (그룹별)
# boxplot(data$수치형 ~ data$범주형) or boxplot(수치형 ~ 범주형, data = dataName)
boxplot(sample1$AMT17 ~ sample1$Gender, 
        ylim = c(0, 1500000),
        main = "성별에 따른 카드사용 BOX Plot",
        xlab = "성별",
        ylab = "2017카드사용금액",
        names = c("여성", "남성"),
        col = c("yellow", "green"))

# 상관관계 분석 (수치형변수)
# cor(변수1, 변수2, ...)
cor(sample1[, c(5:8)])

# 상관관계 분석 - 그래프 (수치형)
# plot(변수1, 변수2) - 기본
plot(sample1[,c(5:8)])
plot(sample1$AMT17, sample1$Y17_CNT,
     main = "2017년 카드사용액과 사용건수",
     xlab = "사용액",
     ylab = "사용건수",
     xlim = c(0,1500000),
     cex = 3,
     pch = 10,
     col = "red")
