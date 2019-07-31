# chap10_DescriptiveStatistics

####################################################
##  Chapter10. 기술 통계(Descriptive Statistics)     
####################################################

# 1. 기술 통계 개요.

# 대표값:평균(Mean), 합계(Sum), 중위수(Median), 최빈수(mode), 사분위수(quartile) 등.

# 산포도:분산(Variance), 표준편차(Standard Deviation), 최소값(Minimum), 최대값(Maximum), 범위(Range) 등.

# 비대칭도:왜도(Skewness), 첨도(Kurtosis)


## 1.1 평균과 분산 그리고 표준편차 
score1 <- c(85, 90, 93, 86, 82)
score2 <- c(85, 90, 93, 46, 42)
score3 <- c(100, 100, 54, 50, 52)

# 평균 
mean(score1) # 87.2
mean(score2) # 71.2
mean(score3) # 71.2


# 중앙값(median:중위수) - 모든 데이터를 크기 순서대로 정렬시킨 후 가운데 있는 값을 의미.
# ex) 100, 100, 54, 50, 52 : 중앙값 -> 54
median(score3) # 54

# ex) 6, 6, 7, 8, 9, 10
num <- c(6, 6, 7, 8, 9, 10)
median(num) # 7.5(=(7+8)/2)

# 편차(Deviation) : 평균값을 기준으로 각 값의 차이, 즉, 평균과 측정값의 차이.

# 제곱평균(평균제곱):편차 값을 제곱해서 마이너 값을 플러스 값으로 바꾼 후 평균을 구하는 방법.
# ex) ((100-71.2)^2+(100-71.2)^2+(54-71.2)^2+(50-71.2)^2+(52-71.2)^2) / 5 = 554.56

# 분산(Variance) : 편차 값을 제곱해서 나온 값. 
var(score3) # 693.2
sqrt(693.2)

# 표준편차(Stadard Deviation:SD):분산 값에 루트를 적용해서 제곱을 제거한 값. 
sqrt(554.56) # 23.55
sd(score3) # 26.32869


# 자유도(degree of freedom):표본의 분산과 표준편차를 계산할 때 나누는 분모의 수를 (모집단-1)개로 계산하여 주어진 데이터에서 표본을 자유롭게 뽑을 수 있는 경우의 수를 의미. 표본을 추출해서 표본의 분산과 표준편차를 계산할 때는 항상 자유도를 분모로 사용함.


# 표준화와 표준값 
# 1) 표준화:모든 값들의 표준값을 정해서 그 값을 기준으로 차이를 구해서 비교하는 방법.
# 2) 표준값 = (각데이터 - 평균) / 표준편차 
# 3) 편차값 = 표준값 * 10 + 50

# 2. 척도별 데이터 가져오기


getwd()
setwd("C:/workspaces/R/data")

data <- read.csv("descriptive.csv",header = T)

dim(data)

# 데이터 특성
summary(data)

# 2.1 명목척도 기술 통계량

gender <- data$gender
table(gender)

data <- subset(data,data$gender == 1 | data$gender == 2)

x <- table(data$gender)
barplot(x)

# 구성비율 계산
y<- prop.table(x)
round(y*100, 2)

# 학력수준 변수 대상 구성비율 구하기
level <- data$level
level <- table(level)
barplot(level)# 명목/서열 척도 -> 막대차트
pie(level)

# 2.3 등간척도 기술 통계량

survey <- data$survey
survey
summary(survey) # 빈도수 의미를 가짐
x1 <- table(survey)
x1

pie(x1)
hist(survey)


# 2.4 비율척도 기술 통계량

cost <- data$cost
length(data$cost)
summary(data$cost)

# 데이터 정제 
plot(cost)
data <- subset(data,data$cost >=2 & data$cost <= 10)
x<- data$cost
length(x)
mean(x) #5.354032
summary(data$cost)
x

# 평균이 극단치에 영향을 받는 경우  - 중위수(median) 대체

median(x) # 5.4



























