# chap11_CrossTableChiSquare

#######################################
##  Chapter11. 교차분석과 카이제곱검정      
#######################################

## 1. 교차분석(Cross Table Analyze)
#   - 범주형 자료(명목척도 또는 서열척도)를 대상으로 두 개 이상의 변수들에 대한 관련성 체크.
#   - 결합분포를 나타내는 교차분할표를 작성.
#   - 변수 상호간의 관련성 여부를 분석하는 방법.
#   - 빈도분석의 특성별 차이를 분석하기 위해 수행하는 분석 방법.
#   - 빈도분석결과에 대한 보충자료를 제시하는 데 효과적.
#   - 빈도분석과 함께 고급 통계 분석의 기초 정보를 제공.

# 교차분석 고려사항
# - 교차 분석에 사용되는 변수는 값이 10 미만인 범주형 변수여야 함.
# - 비율척도인 경우는 코딩변경(리코딩)을 통해서 범주형 자료로 변환.
# -  ex) 나이: 10~19세는 1, 20~29세는 2, 30~39세는 3... 


# 1.1 데이터프레임 생성 

# 변수 리코딩과 데이터프레임 생성 

# 1) 실습 파일 가져오기 
data <- read.csv("C:/workspaces/R/data/cleanDescriptive.csv", header=T)
View(data)


# 2) 리코딩 변수 가져오기 
x <- data$level2 # 리코딩 변수 이용(학력수준)
y <- data$pass2 # 리코딩 변수 이용(합격/불합격)
x; y # 부모 학력 수준(x) -> 자녀 대학 진학여부(y)

# 3) 전처리 - 데이터프레임 생성 
result <- data.frame(Level=x, Pass=y) # 데이터 프레임생성-데이터 묶음.

dim(result) # 248 2
head(result)


# 1.2 교차분석 

# 1) 교차분할표 작성 
table(result) # 빈도보기 

#          Pass
#Level      실패 합격
# 고졸       40   49
# 대졸       27   55
# 대학원졸   23   31

# 2) 교차분할표 생성을 위한 패키지 설치 
install.packages("gmodels")
library(gmodels)

# 3) 패키지를 이용한 교차 분할표 생성
CrossTable(x,y)

# 4) 교차테이블에 카이검정 적용 
CrossTable(x,y,chisq=T)
# Pearson's Chi-squared test 
# ----------------------------------------------------
# Chi^2 =  2.766951     d.f. =  2     p =  0.2507057 


# 2. 카이제곱 검정 
#  1) 일원카이제곱 검정
#   (1) 적합성(도) 검정 

#------------------
# 귀무가설(영가설):기대치와 관찰치는 차이가 없다. : p >= 유의수준(알파)
# 연구가설(대립가설):기대치와 관찰치는 차이가 있다. : p < 유의수준(알파)

# 예제) 60회 주사위를 던져서 나온 관측도수/기대도수
#   눈금  :  1  2  3  4  5  6
# 기대도수: 10 10 10 10 10 10
# 관측도수:  4  6 17 16  8  9 

chisq.test(c(4,6,17,16,8,9))
# X-squared = 14.2, df = 5, p-value = 0.01439


#   (2) 선호도 검정(분석)
#      - 분석에 필요한 연구 환경과 자료의 차이점.

#--------------------------------------------
# 귀무가설: 기대치와 관찰치는 차이가 없다.
#   예제) 스포츠음료의 선호도에 차이가 없다. 

# 연구가설:기대치와 관찰치는 차이가 있다.
#   예제) 스포츠음료의 선호도에 차이가 있다.


data <- textConnection(
  "스포츠음료종류   관측도수
        1              41
        2              30
        3              51
        4              71
        5              61
  ")

x <- read.table(data, header=T)
x
str(x)

chisq.test(x$관측도수)
# data:  x$관측도수
# X-squared = 20.488, df = 4, p-value = 0.0003999


#  2) 이원카이제곱 검정 - 교차분할표 이용 
#   (1) 독립성 검정(관련성 검정)

#귀무가설 : 부모의 학력수준과 자녀의 대학진학 여부와 관련성이 없다.
#     - 두 변인은 독립적이다.
#대립가설 : 부모의 학력수준과 자녀의 대학진학 여부와 관련성이 있다.
#     - 두 변인은 독립적이지 않다.

data <- read.csv("C:/workspaces/R/data/cleanDescriptive.csv", header=T)

x <- data$level2 # 리코딩 변수 이용(학력수준)
y <- data$pass2 # 리코딩 변수 이용(합격/불합격)
x; y # 부모 학력 수준(x) -> 자녀 대학 진학여부(y)

result <- data.frame(Level=x, Pass=y) # 데이터 프레임생성-데이터 묶음.

CrossTable(x,y,chisq=T)
# Pearson's Chi-squared test 
# ----------------------------------------------------
# Chi^2 =  2.766951     d.f. =  2     p =  0.2507057 


#   (2) 동질성 검정

# 귀무가설 : 교육방법에 따라 만족도에 차이가 없다.
# 연구가설 : 교육방법에 따라 만족도에 차이가 있다. 

getwd()
data<- read.csv("homogenity.csv",header = T)

head(data)
str(data)

# 데이터 전처리
data <- subset(data,!is.na(survey),c(method,survey))

# 2. 변수 리코딩
data$method2[data$method==1] <- "방법1"
data$method2[data$method==2] <- "방법2"
data$method2[data$method==3] <- "방법3"

data

data$survey2[data$survey==1] <- "매우만족"
data$survey2[data$survey==2] <- "만족"
data$survey2[data$survey==3] <- "보통"
data$survey2[data$survey==4] <- "불만족"
data$survey2[data$survey==5] <- "매우불만족"

head(data)
View(data)
# 3. 교차분석 작성
table(data$method2,data$survey2)

# 4. 교차분할표 생성
CrossTable(data$method2, data$survey2, chisq = T)
# Pearson's Chi-squared test 
# ------------------------------------------------------------
# Chi^2 =  6.544668     d.f. =  8     p =  0.5864574 


# 5. 동질성 검정 - 모수 특성치에 대한 추론 검정
chisq.test(data$method2,data$survey2)
# Pearson's Chi-squared test

# data:  data$method2 and data$survey2
# X-squared = 6.5447, df = 8, p-value = 0.5865 > α


