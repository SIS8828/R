# chap01_Basic : 주석 

##############################
#  Chaper01. R 설치와 개요 
##############################

# 주요 단축키 
# script 실행 : Ctrl+Enter, Ctrl+R
# 저장: Ctrl+S

## 3. 패키지와 Session 보기 

# R 패키지 보기
dim(available.packages()) # 14272
available.packages()

# R 세션보기 
sessionInfo()

# 패키지 사용법
install.packages("stringr") # 패키지 설치 
library("stringr") # 메모리로딩:""생략 가능.
search() # 패키지 메모리 로딩 확인.

# 패키지 제거
remove.packages("stringr")

# 데이터 셋 보기 
data()

# 기본 데이터 셋으로 히스토그램 그리기 
# 단계1:빈도수(frequency)를 기준으로 히스토그램 그리기 
hist(Nile)

# 단계2:밀도(density)를 기준으로 히스토그램 그리기
hist(Nile, freq = F)
# 단계3:단계2의 결과에 분포곡선(line)을 추가.
lines(density(Nile))

# 히스토그램을 파일에 저장하기.
par(mfrow=c(1,1)) # plots 영역에 1개 그래프 표시 
pdf("C:/workspaces/R/output/batch.pdf")
hist(rnorm(20)) # 난수에 대한 히스토그램 그리기 
dev.off()

## 4. 변수와 자료형 
age <- 25
age
age <- 35
age

# 변수.멤버 형태로 변수 선언 예
goods.model <- "lg-320"
goods.name <- "냉장고 "
goods.price <- 850000
goods.desc <- "동급 최고 품질/사양"

var1 <- 50
var2 <- 100

## scalar 변수 사용 예 
name <- "홍길동"
name

# 자료형 / 자료형 확인 
int <- 20  # 숫자형(정수)
is.numeric(int) # TRUE
is.integer(int) # FALSE
is.double(int) # TRUE

castingInt <- as.integer(int)
is.integer(castingInt)

is.character(int) # FALSE


double <- 3.14
string <- "홍길동"
boolean <- TRUE # 진리값:TRUE(T)/FALSE(F)
boolean

boolean <- 3.14
boolean

is.character(string) # TRUE
is.character(boolean) # FALSE
is.character("boolean") # TRUE

# 문자 원소를 숫자 원소로 형변환 
x <- c(1, 2, 3)
x # 1 2 3

result <- x * 3
result # 3 6 9

y <- c(1, 2, "3")
y # "1" "2" "3"

result <- y * 5 # Error in y * 5 : non-numeric argument to binary operator

result <- as.integer(y) * 5
result # 5 10 15

# 복소수형 자료 생성과 형변환 
z <- 5.3 - 3i
Re(z)
Im(z)
is.complex(z) # TRUE
as.complex(5.3) # 5.3+0i

# 스칼라 변수의 자료형
mode(int)     # "numeric"
mode(string)  # "character"
mode(boolean) # "logical"

# 문자 벡터와 그래프 생성 
gender <- c('man', 'woman', 'woman', 'man', 'man')
gender

mode(gender)  # "character"
class(gender) # "character"

plot(gender) # error

# as.factor() 함수 이용 범주(요인)형변환 
Ngender <- as.factor(gender)
Ngender
table(Ngender)

# factor형 변수로 차트 그리기 
plot(Ngender)
mode(Ngender)  # "numeric"
class(Ngender) # "factor"
is.factor(Ngender) # TRUE

# Factor Nominal 변수 
Ngender
#[1] man   woman woman man   man  
#Levels: man woman

# factor() 함수 이용 Factor형변환 
args(factor)
Ogender <- factor(gender, levels = c('woman', 'man'), ordered = TRUE)
Ogender

# 순서 없는 factor형과 순서있는 factor형 변수로 차트 그리기 
par(mfrow=c(1, 2))
plot(Ngender)
plot(Ogender)

# 도움말 보기 
i <- sum(1, 2, 3)
i

help(sum)
?sum

# 함수 파라메터 보기
args(sum)

# 함수 사용 예제 보기
example(sum)

# 작업 공간 지정 
getwd()
setwd("C:/workspaces/R/output")
