library(MASS)
data(Animals)
head(Animals)

dim(Animals)
str(Animals)
length(Animals)

summary(Animals)

summary(Animals$brain)

x <- Animals$brain

mean(x) # 평균
median(x) # 중위값
var(x) #분산
sd(x) #표준편차
min(x) # 최소값
max(x) # 최대값값

summary(x)

describe(Animals)
freq(Animals)

#2
data2<- read.csv("descriptive.csv",header = T)

z1<-table(data2$type)
z2<-table(data2$pass)

barplot(z1)
barplot(z2)

pie(z1)
pie(z2)
