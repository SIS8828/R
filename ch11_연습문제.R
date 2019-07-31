# 1번

library(MASS)
data(Animals)
head(Animals)

dim(Animals)
str(Animals)
str(Animals$brain)
summary(Animals$brain)
mean(Animals$brain)
median(Animals$brain)
sd(Animals$brain)
var(Animals$brain)
max(Animals$brain)
min(Animals$brain)

library(Hmisc)
describe(Animals)
library(prettyR)
freq(Animals)

# 2번

getwd()
setwd("C:/workspaces/R/data")

data <- read.csv("descriptive.csv",header = T)
View(data)

head(data)
type <- data$type
pass <- data$pass

summary(data)

x <- table(type)
y <- table(pass)

x1<-prop.table(x)
y1 <- prop.table(y)
round(x1*100,2)
round(y1*100,2)

pie(x1)
pie(y1)
barplot(x1)
barplot(y1)

age <- data$age

summary(age)
sd(age)
var(age)

library(moments)
skewness(age) # 왜도 [1] 0.3804892
kurtosis(age) # 척도 [1] 1.866623
hist(age, freq = F) #히스토그램
lines(density(age), col = 'blue')
x <- seq(40,70,0.1)
curve(dnorm(x,mean(age),sd(age)),col='red',add=T)
# age의 첨도는 정규분포 곡선보다 다소 낮고 양의 비대칭도를 나타내고 있다.