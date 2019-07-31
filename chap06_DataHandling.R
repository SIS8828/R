# chap06_DataHandling

################################
##  Chapter06. 데이터 조작  
################################

## 1. plyr 패키지 활용 
#  - 두 개 이상의 데이터프레임을 대상으로 key값을 이용하여 하나의 패키지로 병합(merge)하거나, 집단변수를 기준으로 데이터프레임의 변수에 함수를 적용하여 요약집계 결과를 제공하는 패키지.

# 1.1 데이터 병합 
# 데이터 병합하기 
install.packages("plyr") # 패키지 설치 
library(plyr) # 패키지 메모리에 로딩 

# 병합할 데이터프레임 셋 생성 
x <- data.frame(id=c(1:5), height=c(160,171,173,162,165))
y <- data.frame(id=c(5,4,1,3,2), weight=c(55,73,60,57,80))


z <- join(x,y,by='id') # id 컬럼으로 조인 
z

# 왼쪽(Left) 조인하기 
x <- data.frame(id=c(1,2,3,4,6), height=c(160,171,173,162,165))
y <- data.frame(id=c(5,4,1,3,2), weight=c(55,73,60,57,80))


left <- join(x,y,by="id") # id 컬럼으로 조인(왼쪽변수기준:NA(결측치:누락된 값, 비어있는 값))
left


# 내부(Inner) 조인하기 
x <- data.frame(id=c(1,2,3,4,6), height=c(160,171,173,162,165))
y <- data.frame(id=c(5,4,1,3,2), weight=c(55,73,60,57,80))
inner <- join(x,y,by="id", type="inner") # type="inner": 값이 있는 것만 조인.
inner


# 전체(Full) 조인하기 
x <- data.frame(id=c(1,2,3,4,6), height=c(160,171,173,162,165))
y <- data.frame(id=c(5,4,1,3,2), weight=c(55,73,60,57,80))
full <- join(x,y,by="id", type="full") # type="full": 모든 항목 조인.
full

# key값으로 병합하기 
x <- data.frame(key1=c(1,1,2,2,3),key2=c('a','b','c','d','e'), val1=c(10,20,30,40,50))
y <- data.frame(key1=c(3,2,2,1,1),key2=c('e','d','c','b','a'), val2=c(500,400,300,200,100))
xy <- join(x,y,by=c('key1','key2'))
xy

# 1.2 그룹별 기술 통계량 구하기 
# (1) tapply() 함수 이용 
# iris 데이터 셋을 대상으로 tapply() 함수 적용하기 
head(iris)
names(iris)
unique(iris$Species) # 꽃의 종류 보기-3가지 

tapply(iris$Sepal.Length, iris$Species, mean) # 평균
# setosa versicolor  virginica 
#  5.006      5.936      6.588
tapply(iris$Sepal.Length, iris$Species, sd) # 표준편차 
# setosa versicolor  virginica 
#0.3524897  0.5161711  0.6358796


# (2) ddply() 함수 이용 
# 꽃의 종류별(Species)로 꽃받침 길이의 평균 구하기 
avg_df <- ddply(iris, .(Species), summarise, avg=mean(Sepal.Length))
avg_df
str(avg_df)

# 꽃의 종(Species)로 여러 개의 함수 적용하기 
a <- ddply(iris, .(Species), summarise,
           avg=mean(Sepal.Length),
           std=sd(Sepal.Length),
           max=max(Sepal.Length),
           min=min(Sepal.Length))
a

# round() 함수를 적용하여 avg, std 컬럼에 반올림 처리하기.
b <- ddply(iris, .(Species), summarise,
           avg=round(mean(Sepal.Length),2),
           std=round(sd(Sepal.Length),2),
           max=max(Sepal.Length),
           min=min(Sepal.Length))
b


## 2. dplyr 패키지 활용 

install.packages(c("dplyr", "hflights"))
library(dplyr)
library(hflights)

str(hflights)
View(hflights)
# 2.1 콘솔 창의 크기에 맞게 데이터 추출 
#     : 콘솔 창 안에서 한 눈으로 파악하기 
hflights_df <- tbl_df(hflights)
hflights_df


# 2.2 조건에 맞는 데이터 필터링 

# hflights_df를 대상으로 1월2일 데이터 추출하기 
filter(hflights_df, Month==1 & DayofMonth==2) # AND(&)

# 1월 혹은 2월 데이터 추출
filter(hflights_df, Month==1 | Month==2) # OR(|)


# 2.3 컬럼으로 데이터 정렬 
# 년, 월, 출발시간, 도착시간 순으로 오름차순 정렬 
arrange(hflights_df,Year,Month,DepTime,ArrTime)

# 출발시간으로 내림차순 정렬하기 
arrange(hflights_df,Year,Month,desc(DepTime),ArrTime)


# 2.4 컬럼으로 데이터 검색 
# hflights_df에서 년,월,출발시간,도착시간 컬럼 검색하기 
select(hflights_df,Month,DepTime,ArrTime,ArrDelay)

# 컬럼의 범위 지정하기 
select(hflights_df, Year:ArrTime)

# 컬럼의 범위 제외 : Year부터 DayOfWeek 제외 
select(hflights_df, -(Year:DayOfWeek))


# 2.5 데이터 셋에 컬럼 추가 
# 출발 지연시간과 도착지연 시간과의 차이를 계산하는 컬럼추가하기 
mutate(hflights_df, gain=ArrDelay-DepDelay, 
       gain_per_hour=gain/(AirTime/60))

# mutate() 함수에 의해서 추가된 컬럼보기 
select(mutate(hflights_df, gain=ArrDelay-DepDelay, 
              gain_per_hour=gain/(AirTime/60)), 
       Year,Month,ArrDelay,DepDelay,gain,gain_per_hour)

# 2.6 요약 통계치 계산 
# 비행시간 평균 계산하기 
summarise(hflights_df, avgAirTime=mean(AirTime,na.rm = T))

# 데이터 셋의 관측치 길이, 출발 지연 시간 평균 구하기 
summarise(hflights_df, cnt=n(), delay=mean(DepDelay,na.rm = T))

# 도착시간(ArrTime)의 표준편차와 분산 계산하기.
summarise(hflights_df, arrTimeSd=sd(AirTime,na.rm = T), arrTimeVar=var(AirTime, na.rm = T))


# 2.7 집단변수 대상 그룹화 

# 집단변수를 이용하여 그룹화하기 
species <- group_by(iris, Species)
str(species)

tailNum <- group_by(hflights_df, TailNum)
tailNum


# 예시)
getwd()
setwd("C:/workspaces/R/data")

exam <- read.csv("csv_exam.csv")
exam


# filter()
exam %>% filter(class==1) # %>%: 파이프연산자(pipe 연산자)

# select()
exam %>% select(class, math, english)

# class가 1인 행만 추출한 다음 english 추출 
exam %>% filter(class==1) %>% select(english)
# 혹은 
exam %>% 
  filter(class==1) %>% 
  select(englis)

# (과목별) 총점과 총점 기준 정렬해서 상위 6개 데이터만을 출력.
exam %>% 
  mutate(total=math+english+science) %>% 
  arrange(desc(total)) %>% 
  head


## 3. reshape 패키지 활용 

# 3.1 컬럼명 변경 

# 단계1 : 패키지 설치 
install.packages("reshape")
library(reshape)

# 단계2 : 실습 데이터 셋 가져오기 
result <- read.csv("C:/workspaces/R/data/reshape.csv",header = F)
result

# 단계3 : 컬럼명 변경 
result <- rename(result, c(V1="total",V2="num1",V3="num2",V4="num3"))
result

# 3.2 긴 형식을 넓은 형식으로 변경 
data("Indometh")
str(Indometh) # Class 'formula'  language conc ~ time | Subject

Indometh # 긴 형식 보기 
range(Indometh$Subject)
#[1] 1 3
#Levels: 1 < 4 < 2 < 5 < 6 < 3

# 긴형식 <- 넓은 형식 
help("reshape")

#reshape(data.frame,
#        v.names = "반복되는 측정 값", 
#        timevar = "반복되는 측정 시간",
#        idvar = "1개 이상의 값으로 분류되는 변수",
#        direction = "wide/long")


wide <- reshape(Indometh,
                v.names = "conc",
                timevar = "time",
                idvar = "Subject",
                direction = "wide")

wide

long <- reshape(wide, direction = "long")
long


## 4. reshape2 패키지 활용 

# 4.1 긴 형식을 넓은 형식으로 변경 

# 패키지 설치와 데이터 가져오기 
install.packages("reshape2")
library(reshape2)

# 데이터 가져오기
data <- read.csv("data.csv")
data

# '넓은 형식(wide format)'으로 변경 
wide <- dcast(data, Customer_ID ~ Date, sum)
wide

# 파일 저장 및 읽기 
setwd("C:/workspaces/R/output")
write.csv(wide, 'wide.csv', row.names = F)

wide_read <- read.csv("wide.csv")
colnames(wide_read) <- c("Customer_ID","day1","day2","day3","day4","day5","day6","day7")

wide_read


# 4.2 넓은 형식을 긴 형식으로 변경 

# melt() 함수 이용 : '넓은 형식'을 '긴 형식'으로 변경하기 
long <- melt(wide_read, id='Customer_ID')
long

# 컬럼명 수정
colnames(long) <- c("Customer_ID", "Date", "Buy")
head(long)


# reshape2 패키지의 smiths 데이터 셋 구조 변경하기 
data("smiths")
smiths

# wide -> long
long <- melt(smiths, id=1:2)
long

# long -> wide
wide <- dcast(long, subject+time ~ ...)
wide
str(wide)


# 4.3 3차원 배열 형식으로 변경 

# airquality 데이터셋 구조 변경 
data("airquality")
airquality
str(airquality)

# 컬럼명 대문자 일괄 변경 
names(airquality) <- toupper(names(airquality))
head(airquality)

# 월과 일 컬럼으로 나머지 4개 컬럼을 묶어서 긴 형식 변경.

air_melt <- melt(airquality, id=c("MONTH","DAY"), na.rm = T)
head(air_melt) # MONTH DAY variable value

# 월과 일 컬럼으로 variable 컬럼을 3차원 형식으로 분류 
names(air_melt) <- tolower(names(air_melt)) # 컬럼명 소문자 변경 
acast <- acast(air_melt, day~month~variable) # 3차원 구조 변경 
acast

# 월 단위 variable(오존,태양열,바람,온도) 컬럼 합계
acast(air_melt, month~variable, sum, margins=T)












