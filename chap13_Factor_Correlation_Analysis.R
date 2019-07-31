# chap13_Factor_Correlation_Analysis

###########################################
## Chapter_13.1 요인분석(Factor Analysis)##
###########################################

# 요인분석 결과에 대한 활용 방안
# 1. 서로 밀접하게 관련된 변수들을 합치거나 중복된 변수를 제거하여 변수를 축소한다. 

# 요인분석 결과에 대한 활용 방안
# 1. 서로 밀접하게 관련된 변수들을 합치거나 중복된 변수를 제거하여 변수를 축소한다. 
# 2. 변수들 간의 연관성 또는 공통점 탐색
# 3. 요인 점수 계산으로 상관분석, 회구분석의 설명변수로 이용. 

# 요인분석에 사용될 데이터 셋

# 1.1 공통요인으로 변수 정제

# 변수와 데이터 프레임 생성

# 과목 변수 생성
# s1 : 자연과학, s2: 물리화학
# s3 : 인문사회, s4: 신문방송
# s5 : 응용수학, s6: 추론통계

s1 <- c(1,2,1,2,3,4,2,3,4,5)
s2 <- c(1,3,1,2,3,4,2,4,3,4)
s3 <- c(2,3,2,3,2,3,5,3,4,2)
s4 <- c(2,4,2,3,2,3,5,3,4,1)
s5 <- c(4,5,4,5,2,1,5,2,4,3)
s6 <- c(4,3,4,4,2,1,5,2,4,2)
name <- 1:10
subject <- data.frame(s1,s2,s3,s4,s5,s6)
subject

str(subject)
summary(subject)

# 변수의 주요 성분 분석
# - 주성분 분석 : 변동량(분산)에 영향을 주는 주요 성분을 분석하는 방법. 요인분석에서 사용될 요인의 개수를 결정하는데 이용
pc <- prcomp(subject)
summary(pc)

plot(pc)

# 고유값으로 요인 수 분석
en<- eigen(cor(subject))
names(en)

en$values
en$vectors

plot(en$values,type = "o")

cor(subject)

# 요인분석: 요인분석법 적용 (varimax is the default)

# (1) 주성분분석의 가정하에 의해서 2개 요인으로 분석
result <- factanal(subject,factors = 2,rotation = "varimax")
result
# 0.0232  < 0.05 (요인수 부족)

# (2) 주성분분석의 가정하에 의해서 3개 요인으로 분석
result <- factanal(subject,factors = 3,rotation = "varimax")
result
# 0.7745 > 0.05 (요인수)


## 1.2 잘못 분류된 요인 제거로 변수 정제
# 요인분석에 사용될 데이터 셋 가져오기

# (1) 데이터 가져오기
install.packages("memisc")
library(memisc)

data.spss <-as.data.set(spss.system.file("C:/workspaces/R/data/drinking_water.sav"))

head(data.spss)

str(data.spss)
dim(data.spss)
summary(data.spss)


# (2) 데이터프레임으로 변경
drinking_water <- data.spss[1:11]
drinking_water_df <- as.data.frame(data.spss[1:11])
str(drinking_water_df)

# (3) 요인수를 3개로 지정하여 요인분석 수행
result2 <- factanal(drinking_water_df, factors = 3)

print(result2, cutoff=0.5)

# 요인별 변수 묶기
# 1) q4 컬럼 제외하여 데이터프레임 생성
dw_df <- drinking_water_df[-4]
dw_df

cor(drinking_water_df)
cor(dw_df)


# 2) 요인에 속하는 입력 변수별 데이터프레임 구성


# 제품만족도 저장
s <- data.frame(dw_df$q8,dw_df$q9,dw_df$q10,dw_df$q11)

# 제품친밀도 저장
c <- data.frame(dw_df$q1,dw_df$q2,dw_df$q3)
# 제품적절성
p <- data.frame(dw_df$q5,dw_df$q6,dw_df$q7)

# 3) 요인별 산술평균 계산
satisfaction <- round((s$dw_df.q8+s$dw_df.q9+s$dw_df.q10+s$dw_df.q11)/ncol(s),2)
closeness <- round((c$dw_df.q1+c$dw_df.q2+c$dw_df.q3)/ncol(c),2)
pertinence <- round((p$dw_df.q5+p$dw_df.q6+p$dw_df.q7)/ncol(p),2)

# 4) 상관관계 분석
drinking_water_factor_df <- data.frame(satisfaction,closeness,pertinence)
colnames(drinking_water_factor_df) <- c("제품만족도","제품친밀도","제품적절성")
cor(drinking_water_factor_df)


################################################
## Chapter_13.2 상관분석(Corrleation Analysis)##
################################################

# 1. 상관관계 분석 수행

result <- read.csv("product.csv",header = T)
head(result)
str(result) #'data.frame':	264 obs. of  3 variables:

# 기술통계량
summary(result)

sd(result$제품_친밀도)
sd(result$제품_적절성)
sd(result$제품_만족도)

# 상관계수(coefficient of corrlation) : 두 변량 x,y 사이으 ㅣ상관관계 정도를 나타내는 수치

cor(result$제품_친밀도,result$제품_적절성)
cor(result$제품_친밀도,result$제품_만족도)

# 적절성 + 친밀도 -> 만족도 상관계수 보기
cor(result$제품_적절성+result$제품_친밀도,result$제품_만족도)



# 방향성 있는 색상을 표현
install.packages("corrgram")
library(corrgram)

corrgram(result)
corrgram(result,upper.panel = panel.conf)
corrgram(result,lower.panel = panel.conf)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

chart.Correlation(result,histogram = ,pch="+")















