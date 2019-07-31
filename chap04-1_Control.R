## chap04-1_Control

#####################################################
##  Chapter04-1. 연산자 / 제어문(조건문과 반복문)
#####################################################

###############
## 1. 연산자
###############

# 산술연산자
num1 <- 100 # 피연산자1
num2 <- 20  # 피연산자2

result <- num1 + num2  # 덧셈
result
result <- num1 - num2  # 뺄셈
result
result <- num1 * num2  # 곱셈
result
result <- num1 / num2  # 나눗셈
result

result <- num1 %% num2  # 나머지 연산자
result

result <- num1^2   # 제곱연산자(num1 ** 2)
result

result <- num1^num2   # 100의 20승(10의 40승과 동일한 결과)
result   # [1] 1e+40 -> 1 * 10^40


# 비교(관계) 연산자
# (1) 동등비교
boolean <- num1 == num2 # 두 변수의 값이 같은지 비교 
boolean
boolean <- num1 != num2 # 두 변수의 값이 다른지 비교 
boolean

# (2) 크기비교
boolean <- num1 > num2 
boolean
boolean <- num1 >= num2 
boolean
boolean <- num1 < num2 
boolean
boolean <- num1 <= num2 
boolean

# 논리 연산자
logical <- num1 >= 50 & num2 <= 10
logical

logical <- num1 >= 50 | num2 <= 10
logical

x <- TRUE; y <- FALSE
xor(x, y)
x <- TRUE; y <- TRUE
xor(x, y)

logical <- num1 >= 50
logical

logical <- !(num1 >= 50)
logical


###############################
## 2. 조건문
###############################

# 1) if()
x <- 10
y <- 5
z <- x * y
z

#if(조건식){ 산술/비교/논리 연산자 
#    실행문1 <- 참
#}else{
#    실행문1 <- 거짓
#}

if(x*y > 40){ # 산술 > 비교 > 논리
  cat("x*y의 결과는 40이상입니다.\n")  # \n :줄바꿈
  cat("x*y = ", z, '\n')
  print(z)
}else{
  cat("x*y의 결과는 40미만입니다. x*y=", z, "\n")
}


# 학점 구하기
score <- scan()
score

if(score >= 90){ # 조건식1
  result = "A학점"
}else if(score >= 80){ # 조건식1
  result = "B학점"
}else if(score >= 70){ # 조건식2
  result = "C학점"
}else if(score >= 60){ # 조건식3
  result = "D학점"
}else{
  result = "F학점"
}

cat("당신의 학점은 ", result) # 당신의 학점은?
print(result)


# 2) ifelse(조건, 참, 거짓) - 3항 연산자 기능

score <- c(78, 95, 85, 65)
score
ifelse(score >= 80, "우수", "노력")
## [1] "노력" "우수" "우수" "노력"


# ifelse() 응용
getwd()
setwd("C:/workspaces/R/data")
excel <- read.csv("excel.csv", header = T)
excel

q1 <- excel$q1   # q1 변수값 추출
ifelse(q1>=3, sqrt(q1), q1) # 3보다 큰 경우 sqrt() 함수 적용 


excel <- read.csv("excel.csv", header = T)
excel

q1 <- excel$q1   # q1 변수값 추출
ifelse(q1>=2 & q1<=4, q1^2, q1)


# 3) switch문
#   형식) switch(비교구문, 실행구문1, 실행구문2, 실행구문3)
switch("name", id="hong", pwd="1234", age=25, name="홍길동")

empname <- scan(what="")
empname

switch(empname, hong=250, lee=350, kim=200, kang=400) # [1] 250


# 4) which 문
# - which()의 괄호내의 조건에 해당하는 위치(인덱스)를 출력한다.
# - 벡터에서 사용 -> index값 리턴

name <- c("kim", "lee", "choi", "park")
which(name == "choi") # [1] 3


# 데이터프레임에서 사용
no <- c(1:5)
name <- c("홍길동","이순신","강감찬","유관순","김유신")
score <- c(85, 78, 89, 90, 74)

exam <- data.frame(학번=no, 이름=name, 성적=score)
exam
which(exam$이름=="유관순")

exam[4,]


###############
## 3. 반복문
###############

# 1) 반복문 - for(변수 in 값){ 표현식 }

i <- c(1:10)
i

d <- numeric() # 빈 vector(숫자)

for(n in i){ # 10회 반복
  print(n * 10) 
  print(n)
  d[n] <- n*2
}

d


for(n in i){
  if(n %% 2 != 0){
    print(n) # %% : 나머지값 - 홀수만 출력
  }
}

for(n in i){
  if(n%%2 == 0){
    next # 다음 문장 skip -> 반복문 계속(continue 키워드와 동일)
  }else{
    print(n) # 홀수만 출력
  }
}

# 벡터 데이터 사용 예
score <- c(85, 95, 98)
name <- c("홍길동", "이순신", "강감찬")

i <- 1    # 첨자로 사용되는 변수
for(s in score){
  cat(name[i], " -> ", s, "\n")
  i <- i + 1
}

# 2) 반복문 - while(조건){표현식}

i = 0

while(i < 10){
  i <- i + 1
  print(i)
}



