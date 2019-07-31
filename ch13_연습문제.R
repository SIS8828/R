# 1

getwd()
setwd("C:/workspaces/Rwork/Part-III")
hdtv <- read.csv("hdtv.csv",header = T)

head(hdtv)

id <- hdtv$user.id
buy <- hdtv$buy
id;buy

table(buy,id,useNA = "ifany")
table(buy)

summary(hdtv)
length(buy)
table(buy)

library(prettyR)
freq(buy)

binom.test(c(10,40),p=0.15)
binom.test(c(10,40),p=0.15,alternative = "two.side",conf.level = 0.95)
# p-value = 0.321

binom.test(c(10,40),p=0.15,alternative = "less",conf.level = 0.95)
# p-value = 0.8801

binom.test(c(10,40),p=0.15,alternative = "greater",conf.level = 0.95)
# p-value = 0.2089
# 양측에 차이가 없고 그러므로 방향성도 x

# 2번

stheight <- read.csv("student_height.csv",header = T)
height <- stheight$height

summary(stheight)
length(height)

table(height)

mean(height)
shapiro.test(height)

wilcox.test(height, mu=148.5)




