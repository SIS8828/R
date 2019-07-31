# chap07_DataPreprocessing

################################
##  Chapter07. 데이터 전처리   
################################

# - 자료분석에 필요한 데이터를 대상으로 불필요한 데이터를 처리하는 필터링과 전처리 방법에 대해서 알아본다. 


# 1. 탐색적 데이터 셋 조회

# 데이터 셋 보기
getwd()
setwd("C:/workspaces/R/data")

dataset<- read.csv("dataset.csv", header = T)

dataset

# 데이터 조회
# -탐색적 데이터 분석을 위한 데이터 조회

# 전체 데이터 보기
print(dataset)
dataset$age

View(dataset)
head(dataset)
tail(dataset)
head(dataset,10)

# 데이터 구조 보기
names(dataset)
str(dataset)
attributes(dataset)

# 데이터 셋 조회
dataset$age
dataset$resident
length(dataset$age) #d data 수(행)

#조회 결과 변수 저장

x <- dataset$gender
y <- dataset$price

# 산점도 형태로 변수 조회
plot(x,y) # 극단치 발견

# 산점도 형태로 변수 조회
plot(dataset$price)

# ["컬러명"] 형식으로 특정 변수 조회
dataset["gender"]
dataset["price"]

# [색인(index)]형식으로 특정 변수 조회
dataset[2]
dataset[6]
dataset[3,] # 3번째 행 전체
dataset[,3] # 3번째 열 전체 #출력형태: 행 중심
# [1]  1  2  2 NA  3  2  1  2  1  2 NA  3  1  3  2  1  2  3  2 ...

# 두개 이상의 [색인(index)] 형식으로 변수 조회
dataset[c("job","price")]
dataset[c(-1:-3)]

# dataset 특정 행/열을 조회하는경우
dataset[,c(2:4)] # = dataset[2:4]
dataset[c(2:4),]


# 2. 결측치(NA) 처리

summary(dataset$price)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-457.200    4.425    5.400    8.752    6.300  675.000 
#NA's 
# 30 
sum(dataset$price)

# 결측치 제거
# sum() 함수에서 제공되는 속성이용
sum(dataset$price, na.rm = T)
#[1] 2362.9

# 결측데이터 제거 함수 이용
price2<-na.omit(dataset$price)
sum(price2)
length(price2)

# 결측치 대체
# 결측치를 0으로 대체하기 
x <- dataset$price 
x[1:30]

ifelse(!is.na(x), x, 0)

# 결측치를 평균으로 대체하기
x <- dataset$price 
x[1:30]

dataset$price3 <- ifelse(!is.na(x),x,round(mean(x,na.rm = T),2)) #평균으로 대체
dataset$price3[1:30]


head(dataset[c("price","price2","price3")],30)
View(dataset)


# 3. 극단치(이상치) 처리

# 3.1 범주형 극단치 처리

# 범주형 변수의 극단치 확인
table(dataset$gender) # 빈도수
# 0   1   2   5 
# 2 173 124   1 
pie(table(dataset$gender))

# subset() 함수를 이용한 데이터 정제하기
dataset <- subset(dataset, dataset$gender == 1 | dataset$gender == 2)
table(dataset$gender) # 0,5 극단치 값 사라짐
# 1   2 
# 173 124

length(dataset$gender)

pie(table(dataset$gender))

# 3.2 연속형 변수의 
dataset <- read.csv('dataset.csv',header = T)
dataset$price
length(dataset$price) # 300
plot(dataset$price) # 산점도도
summary(dataset$price)

# price 변수의 데이터 정제와 시각화
dataset2 <- subset(dataset,price >= 2 & price <= 8)
length(dataset2$price)
#[1] 251      49개의 데이터가 정제됨

stem(dataset2$price) # 줄기와 잎 도표보기
# 줄기 | 잎 
#    2 | 133
#    2 | 
#    3 | 0000003344
#    3 | 55555888999
#    4 | 000000000000000111111111222333334444
#    4 | 566666777777889999
#    5 | 00000000000000000011111111111222222222333333344444
#    5 | 55555555566667777778888899
#    6 | 00000000000000111111112222222222222333333333333333344444444444
#    6 | 55557777777788889999
#    7 | 000111122
#    7 | 777799


# age 변수에서 NA 발견 
summary(dataset2$age)
length(dataset2$age) # 251

dataset2 <- subset(dataset2,age >= 20, age <= 69)
length(dataset2$age) # 251-235 = 16
summary(dataset2$age)

boxplot(dataset2$age)

# 4. 코딩 변경

table(dataset2$resident)
# 1   2   3   4   5 
# 103  47  23  13  34 

# 가독성을 위한 코딩 변경
dataset2$resident2[dataset2$resident== 1] <- '1. 서울특별시'
dataset2$resident2[dataset2$resident== 2] <- '2. 인천광역시'
dataset2$resident2[dataset2$resident== 3] <- '3. 대전광역시'
dataset2$resident2[dataset2$resident== 4] <- '4. 대구광역시'
dataset2$resident2[dataset2$resident== 5] <- '5. 시구군'
dataset2[c("resident","resident2")]
View(dataset2)

#job 컬럼을 대상으로 코딩 변경하기
table(dataset2$job)
# 1  2  3 
# 59 84 80 

dataset2$job2[dataset2$job == 1] <- '1.공무원'
dataset2$job2[dataset2$job == 2] <- '2.회사원'
dataset2$job2[dataset2$job == 3] <- '3.개인사업자'

dataset2[c("job","job2")]


# 척도 변경을 위한 코딩변경

# 나이 변수를 청년층, 중년층, 장년층으로 코딩 변경하기

dataset2$age2[dataset2$age <= 30] <- "청년층"
dataset2$age2[dataset2$age > 30 & dataset2$age <= 55] <- "중년층"
dataset2$age2[dataset2$age > 55] <- "장년층"

head(dataset2[c("age","age2")])


# 역코딩을 위한 코딩 변경

# 만족도를 긍정적 순서로 역코딩

survey <- dataset2$survey
csurvey <- 6 - survey
csurvey

dataset2$survey <- csurvey
head(dataset2)

# 5. 탐색적 분석을 위한 시각화
#  범주형 vs 범주형

new_data <- read.csv("new_data.csv",header = T)
str(new_data)

# 범주형(resident) vs 범주형(gender)
resident_gender <- table(new_data$resident2,new_data$gender2)
resident_gender

barplot(resident_gender, beside = T, horiz = T,col = rainbow(5), legend = row.names(resident_gender), main = '성별에 따른 거주지역 분포 현황황')

# 거주지역에 따른 성별 분포 현황
gender_resident <- table(new_data$gender2,new_data$resident2)
gender_resident
barplot(gender_resident,beside = T, horiz = T, col= rainbow(2), legend= row.names(gender_resident), main = "거주지역에 따른 성별 분포 현황")

# 연속형(age) vs 범주형(job2)

# 연속형 vs 범주형 데이터 분포 시각화
install.packages("lattice")
library(lattice)

# 직업유형에 따른 나이 분포 현황
summary(new_data$age) # 20~69세

help(densityplot)

densityplot( ~ age, data = new_data,groups = job2, plot.point=T,auto.key = T)


# 연속형(price) vs 범주형(gender) vs 범주형(position)
# 성별에 따른 직급별 구매비용 분포 현황 분석
densityplot(~prirce |factor(gender2),data=new_data,groups = position2,plot.point=T,auto.key = T)

# 직급에 따른 성별 구매비용 분석
densityplot(~ price|factor(position2), data=new_data,groups = gender2,plot.point=T,auto.key = T)

# 연속형 vs 연속형 vs 범주형
# 연속형 (price/age) vs 범주형(gender2)
help(xyplot)
xyplot(price ~ age|factor(gender2), data = new_data)

# 6. 파생변수 생성
# 더미 형식으로 파생변수 생성
#주택유형(단독주택,빌라):0, 아파트유형(아파트,오피스텔):1

#데이터 파일 가져오기
user_data<-read.csv("user_data.csv",header = T)
head(user_data)
table(user_data$house_type)
#  1   2   3   4 
#  32  47  21 300 

# 더미변수 생성
house_type2 <- ifelse(user_data$house_type == 1|user_data$house_type==2,0,1)

house_type2[1:10]
#파생변수 추가
user_data$house_type2 <- house_type2
head(user_data)

# 1:N -> 1:1 관계로 파생변수로 생성
pay_data <- read.csv('pay_data.csv',header = T)
head(pay_data)
table(pay_data$product_type)
#  1   2   3   4   5 
#  55  82  89 104  70 

# 고객별 상품유형에 따른 구매금액 합꼐 파생변수 생성
library(reshape2)
product_price <- dcast(pay_data,user_id~product_type,sum,na.rm=T)

head(product_price,3)

names(product_price) <- c('user_id','식료품(1)','생필품(2)','의류(3)','잡화(4)','기타(5)')

#고객식별번호(id) vs 고객지불유형(pay)method 간의 1:1 파생변수 생성

# 고객별 지불유형에 따른 구매상품 개수 파생변수 생성
pay_price<-dcast(pay_data,user_id~pay_method,length)

head(pay_price)

# 6.3 파생변수 합치기

# 고객 정보 테이블에서 파생변수 추가
library(plyr)
user_pay_data <- join(user_data,pay_data,by='user_id')
head(user_pay_data)

# 병합(위에 결과)를 대상으로 고객별 지불유형에 따른 구매상품 개수 병합하기 

user_pay_data <- join(user_pay_data,pay_price,by='user_id')

user_pay_data[c(1:10),c(1,7:15)]

# 7. 표본 샘플링

# 7.1 정제(cleaning) 데이터 저장하기
print(user_pay_data)
getwd()
setwd("C:/workspaces/R/output")
write.csv(user_pay_data,"cleanData.csv",quote = F,row.names = F)
data<- read.csv("cleanData.csv",header = T)
data

# 7.2 표본 샘플링

#표본추출하기
nrow(data) # 497 -> data의 행수 구하기 
choice1 <- sample(nrow(data), 30) # 30개 무작위 추출

# 50~data 사이에서 30개 무작위 추출
choice2 <- sample(50:nrow(data), 30)

# 50~ 100 사이에서 30개 무작위 추출
choice3 <- sample(50:100,30)

# 다양한 범위를 지정해서 무작위 샘플링
choice4 <- sample(c(10:50,80:150,160:190),30)

data[choice1,1]


# iris 데이터셋을 대상으로 7:3 비율로 데이터 셋 생성
data(iris)
dim(iris)

iris07<- sample(1:nrow(iris),nrow(iris) * 0.7)
training <- iris[iris07,] # 학습데이터 셋
testing <- iris[-iris07,] # 검정데이터 셋

dim(training)
dim(testing)

# 7.3 교차 검정 샘플링
# - 7:3 비율의 모델 평가 방식에 평가의 신뢰도를 높이기 위해서 동일한 데이터 셋을 N등분하여 N-1개의 학습데이터 모델을 생성하고 나머지 1개를 검정데이터로 이용하여 모델을 평가하는 방식

# 데이터 셋을 대상으로 k겹(fold) 교차 검정 데이터 셋 생성
name <- c('a','b','c','d','e','f')
score <- c(90,85,99,75,65,88)
df <- data.frame(Name=name,Score=score)
df

# 교차 검정을 위한 패키지 설치
install.packages("cvTools")
library(cvTools)

cross <- cvFolds(n=6,K=3,R=1,type = "random")
cross

str(cross)

  # which를 이용하여 subsets 데이터 참조
cross$subsets[cross$which == 1, 1] # k=1 인 경우 :1 5
cross$subsets[cross$which == 2, 1] # k=2 인 경우 :3 6
cross$subsets[cross$which == 3, 1] # k=3 인 경우 :4 2

# 데이터프레임의 관측치 적용하기
R <-1  # 1회전
K <- 1:3 # 3겹(fold)
for(i in K){# 3회 반복
  data_idx<-cross$subsets[cross$which==i, R]
  cat('i=',i,'검정데이터\n')
  print(df[data_idx,])
  
  cat('K=',i,'훈련데이터 \n')
  print(df[-data_idx,])
}

