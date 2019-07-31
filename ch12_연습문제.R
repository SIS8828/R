# 1번 -  귀무가설 귀각
smoke <- read.csv("smoke.csv",header = T)

head(smoke)
summary(smoke)

smoke$education[smoke$education == 1] <- "대졸"
smoke$education[smoke$education == 2] <- "고졸"
smoke$education[smoke$education == 3] <- "중졸"

smoke$smoking[smoke$smoking == 1] <- "과다흡연"
smoke$smoking[smoke$smoking == 2] <- "보통흡연"
smoke$smoking[smoke$smoking == 3] <- "비흡연"

CrossTable(smoke$education,smoke$smoking)

chisq.test(smoke$education,smoke$smoking)

# data:  smoke$education and smoke$smoking
# X-squared = 18.911, df = 4, p-value = 0.0008183


# 2번 -  귀무가설 귀각
data2 <- read.csv("cleanData.csv", header = T)
head(data2)

# 전처리
data2 <- subset(data2,!is.na(age),c(age,job))
data2

# 리코딩

data2$position[data2$position==1]<- "임원"
data2$position[data2$job==5|data2$job==4]<- "간부"
data2$position[data2$job==3|data2$job==2|data2$job==1]<- "사원"


data2$age3[data2$age>= 6 & data2$age < 30] <- "청년증"
data2$age3[data2$age>= 30 & data2$age < 50] <- "중년층"
data2$age3[data2$age>= 50 & data2$age < 66] <- "장년층"

x <- data2$position
y <- data2$age3

x
y
summary(data2)

plot(x,y,abline(lm(y~x)),main="나이와 직위에 대한 산점도")


summary(data2)
str(data2$age)
data2$job
View(data2)

CrossTable(data2$position,data2$age3,chisq = T)



# 3번 -  귀무가설 귀각

install.packages("XLConnect")
library(XLConnect)

response <- read.csv("response.csv",header = T)
response
response$job[response$job==1] <- "학생"
response$job[response$job==2] <- "직장인"
response$job[response$job==3] <- "주부"

response$response[response$response==1] <- "무응답답"
response$response[response$response==2] <- "낮음"
response$response[response$response==3] <- "높음"

CrossTable(response$job,response$response)

chisq.test(response$job,response$response)



help("XLConnect")



