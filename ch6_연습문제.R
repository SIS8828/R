library(plyr)
library(ggplot2)
data("mtcars")
#단계1) 데이터 구조 보기
str(mtcars)
head(mtcars)
#단계2) 통계치 계산
d <- ddply(mtcars, .(cyl, gear), summarise,
           mpg.avg = mean(mpg), mpg.std = round(sd(mpg),3) )
d


#2번

library(dplyr)
library(hflights)

str(hflights)
summarise(hflights, avgairtime = mean(AirTime,na.rm = T))
#3번
summarise(hflights,avgairtime = sum(AirTime,na.rm=T)/n())

#4번
summary(hflights)
# AirTime min = 11.0
# NA = 3622 보정
hflights2<- filter(hflights,AirTime >= 11)

summarise(hflights2,avgairtime = sum(AirTime,na.rm=T)/n())
# 5번
summarise(hflights,arsd = sd(ArrTime,na.rm = T),arvar = var(ArrTime,na.rm = T))

# 6번

library(reshape2)
data(iris)
# 1)
m<- melt(iris, id='Species')
m
# 2)
dcast(m, Species ~variable  , sum)

# 3) acast()
# 월과 일 컬럼으로 variable 컬럼을 3차원 형식으로 분류 
names(air_melt) <- tolower(names(air_melt)) # 컬럼명 소문자 변경 
acast <- acast(air_melt, day~month~variable) # 3차원 구조 변경 
acast
# 월 단위 variable(오존,태양열,바람,온도) 컬럼 합계
acast(air_melt, month~variable, sum, margins=T)

acast <- acast(iris$Species, )

acacst <- acast(m, Species~variable, mean,margins = T)
acacst
