# chap12_Ttest_Anova

###########################################
## Chapter12. 집단 간 차이 분석
###########################################

## 0. 추론 통계 분석 분류
# - 추정(estimation) : 표본을 통해서 모집단을 확률적으로 추측.
# - (가설) 검증(hypotheses testing): 유의 수준과 표본의 검정 통계량을 비교하여 통계적 가설의 진위를 입증



## 1. 추정

# 1) 점추정: 제시된 한 개의 값과 검정 통꼐량을 직접 비교하여 가설 기각 유무를 결정
#     ex) 우리나라 중학교 2학년 남학생 평균키는 165.2cm로 추정
# 2) 구간추정: 신뢰구간과 검정통계량을 비교하여 가설 기각 유무를 결정.
#   - 신뢰구간 : 오차범위에 의해서 결정된 하한값과 상한값의 범위


# 예제) 우리나라 중학교 2학년 남학생 평균 신장 표본 조사
#     - 전체 표본 크기(N) : 10,000 명
#     - 표본평균(X) : 165.1 CM
#     - 표본 표준편차(S) : 2 CM

# 모평균 신뢰수중 95%의 신뢰구간(하한값과 상한값) 구하기
N <- 10000
X <- 165.1
S <- 2

# 신뢰구간 하한값
low <- X - 1.96 * S / sqrt(N)

# 신뢰구간 상한값
high <- X + 1.96 * S / sqrt(N)

high

low; high # 165.0608 ~  165.1392

# 해석 : 신뢰수준 95%는 신뢰구간이 모수를 포함할 확률을 의미하고, 신뢰구간은 오차범위에 의해서 결정된 하한값 ~ 상한값을 의미

# 신뢰구간으로 표본오차 구하기
(low - X) * 100
# [1] -0.0392
(high - X) * 100
# [1] 0.0392

# 최종 해석 : 우리나라 중학교 2학년 남학생 평균 신장이 95% 신뢰수준에서 표본 오차 -3.92 ~ 3.92 범위에서 165.1cm로 조사가 되었다면, 실제 평균치는  165.0608 ~ 165.1392 사이에 나타날 수 있따는 의미



## 2. 단일집단 검정 0 한개의 집단과 기존 집단과의 비율/ 평균 차이 검정

# 2.1 단일집단 비율 검정
#     - 기술통계량으로 빈도 수에 대한 비율에 의미.
#     - 단일 집단의 비율이 어떤 특정한 값과 같은지를 검정하는 방법( 검정 방법 중에서 가장 간단).

# 단일 표본 빈도수와 비율 계산


data <- read.csv("one_sample.csv",header = T)
head(data)

x <- data$survey

# 빈도수와 비율 계산
summary(x)
length(x)
table(x)

# 패키지 이용 빈도수와 비율 계산
library(prettyR)
freq(x)

# 가설검정 binom.test() 함수 : 명목척도(y/n) 대상

help(binom.test)

#' 연구가설(H1):기존2014년도고객불만율과2015년도CS교육후불만율에차이가있다.
#' 귀무가설(H0):기존2014년도고객불만족율과2015년도CS교육후불만율에차이가없다.

# 이항분보 비율 검정
# 1) 양측검정
binom.test(c(136,14),p=0.8) # 기존 80% 만족율 기준 검증 실시
#  p-value = 0.0006735

# 해석 결과 : 연구가설 채택 
#  alternative = "two.sided" > 양측검정을 수행하겠다.
#  conf.level = 0.95 > 95%의 신뢰도로 검사하겠다.
binom.test(c(136,14),p=0.8,alternative = "two.sided",conf.level = 0.95)
# p-value = 0.0006735

# 2) 방향성을 갖는 단측검정
binom.test(c(136,14),p=0.8,alternative = "greater",conf.level = 0.95)
#  p-value = 0.0003179
binom.test(c(136,14),p=0.8,alternative = "less",conf.level = 0.95)
#  p-value = 0.9999

# 이항분포 불만족율 기준 비율 검정
# 1) 양측검정
binom.test(c(14,136),p=0.2, alternative = "two.sided",conf.level = 0.95)
# p-value = 0.0006735

# 2) 방향성을 갖는 단측 가성 검정
binom.test(c(14,136),p=0.2, alternative = "greater",conf.level = 0.95)
# p-value = 0.9999
binom.test(c(14,136),p=0.2, alternative = "less",conf.level = 0.95)
# p-value = 0.0003179

# 2.2 단일집단 평균검정(단일표본 T검정)
# : 단일집단의 평균이 어떤 특정한 집단의 평균과 차이가 있는지를 검정하는 방법.
# : 기술통계량으로 표본평균에 의미.


getwd()
setwd("C:/workspaces/R/data")
data <- read.csv("one_sample.csv",header = T)
head(data)
str(data)
View(data)

x <- data$time

# 단계 2 데이터 분포/ 결측치 제거
summary(x)

# 단계 3 데이터 정제
mean(x, na.rm=T) # NA 제외 평균(방법1)
x <- na.omit(x)
mean(x) #[1] 5.556881


#연구가설
#' 연구가설(H1):국내에서 생산된 노트북과 A회사에서 생산된 노트북의 평균 사용시간에 차이가 있다.
#' 귀무가설(H0):국내에서 생산된 노트북과 A회사에서 생산된 노트북의 평균 사용시간에 차이가 없다.

# 단계 4. 정규분포 검정
#  귀무가설: x의 데이터 분포는 정규분포다
shapiro.test(x)
# 	Shapiro-Wilk normality test
# data:  x
# W = 0.99137, p-value = 0.7242
# 정규분포 검정 함수, 표본이 정규분포로부터 추출된 것이지 테스트하기 위한 방법. 이때 귀무가설은 주어진 데이터가 정규분포로부터 표현이라는 것이다.
# p-value > α : 정규분포로 본다.

# 단계5. 정규분포 시각화
hist(x)

# stats 패키지에서 정규성 검정을 위해서 제공되는 시각화 함수.
qqnorm(x)
qqline(x,lty=1,col="blue")

# 단계6. 평균차이 검정
# T-test(T-검정): 모집단에서 ㅊ출한 표본 데이터의 분포형태가 정규분포일 때 수행.
x1 <- x
# 1. 양측검정: x1 객체와 기존 모집단의 평균 5.2
t.test(x1,mu=5.2, alternative = "two.side",conf.level = 0.95)
#p-value = 0.0001417
t.test(x1,mu=5.2)
#p-value = 0.0001417

# 2. 방향성을 갖는 단측가설 검정
t.test(x1, mu=5.2, alternative = "greater",conf.level = 0.95)
# p-value = 7.083e-05

## 3. 두 집단 검정

# 3.1 두 집단 비율 검정

# (1) 집단별 subset 작성과 교차분석
# 단계 1. 실습데이터 가져오기
data<- read.csv("two_sample.csv",header = T)

x <- data$method # 교육방법 (1:PT, 2:Coding)
y <- data$survey # 만족도(0:불만족, 1:만족)

# 데이터확인
table(x)
table(y)

# data 전처리 & 기술통계량 
table(x,y,useNA = "ifany")

prop.test(c(110,135),c(150,150))
#p-value = 0.0003422
#   prop 1    prop 2 
# 0.7333333 0.9000000 

# 단측검정
prop.test(c(110,135),c(150,150), alter="greater", conf.level=0.95)
# 해설) p-value=0.9998 : 방법A가방법B에비해만족도가낮은것으로파악

getwd()
setwd("C:/workspaces/R/data")
data <- read.csv("two_sample.csv",header = T)
data
head(data)
summary(data)

result<- subset(data,!is.na(score),c(method,score))
head(result)
length(result$score)
dim(result)

a <- subset(result,method == 1) # 교육방법별로 분리
b <- subset(result,method == 2) # 교육방법별로 분리
a1 <- a$score
b1 <- b$score

length(a1)
length(b1)

mean(a1)
mean(b1)

var.test(a1,b1) # 동질성 검정
# p-value = 0.3002
# 두집단간 분포의 모양은 동질적이다. 

t.test(a1,b1)
# p-value = 0.0411
# 귀무가설 기각 
t.test(a1,b1,alternative = "greater",conf.level = 0.95,conf.int=T)
t.test(a1,b1,alternative = "less",conf.level = 0.95,conf.int=T)
data <- read.csv("three_sample.csv",header = T)

head(data)

method <- data$method
survey <- data$survey
method;survey

table(method,useNA = "ifany")
table(method,survey,useNA="ifany")
