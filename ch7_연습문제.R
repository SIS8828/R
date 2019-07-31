#1
position <- dataset2$position
position2 <- 6-position 
position2

dataset2 <- position2
dataset2$position2 <- position2
dataset2
#2
summary(dataset2$resident)

dataset2 <- na.omit(dataset2$resident)
#3
dataset2$gender[dataset2$gender == 1] <- "남자"
dataset2$gender[dataset2$gender == 2] <- "여자"
pie(table(dataset2$gender))

#4
dataset2$age3[dataset2$age <= 30] <- 1
dataset2$age3[dataset2$age > 30 & dataset2$age < 55] <- 2
dataset2$age3[dataset2$age >= 55] <- 3

head(dataset2[c("age","age3")])












# 7
# iris 데이터를 이요하여 5겹 2회 반복하는 교차검정데이터를 샘플링하세
idx <- sample(1:nrow(iris), nrow(iris) * 0.7)
training <- iris[idx,]
testing <- iris[-idx,]
# 07. iris 데이터를 이용하여 5겹 2회 반복하는 교차검정 데이터를 샘플링하시오.
# 단계1: K겹 교차검정 데이터 생성
cross <- cvFolds(nrow(iris), K=5, R=2)
# 단계2: K겹 교차검정 데이터 보기
str(cross) # 구조 보기
table(cross$which) # 5겹 빈도수
# 1 2 3 4 5
# 30 30 30 30 30
cross # 5겹 교차검정 데이터 보기
# 단계3: 샘플링 관측치 행번호 추출
# K=1, R=1인 경우 샘플링 관측치 행번호 추출
datas_idx <- cross$subsets[cross$which==1, 1]
datas_idx # test set
length(datas_idx) # 30
train <- iris[-datas_idx, ]
test <- iris[datas_idx, ]
# K=2, R=1인 경우 샘플링 관측치 행번호 추출
datas_idx <- cross$subsets[cross$which==2, 1]
datas_idx # test set
length(datas_idx) # 30
train <- iris[-datas_idx, ]
test <- iris[datas_idx, ]
# K=5, R=2인 경우 샘플링 관측치 행번호 추출
cross$subsets[cross$which==5, 2]
length(cross$subsets[cross$which==5, 2]) # 30
# 단계4: iris 데이터프레임 적용 데이터 셋 생성
# R=1:2
# K=1:5
for(r in R){ # 2회
  cat('R=',r, '\n')
  for(k in K){ # 5회
    datas_idx <- cross$subsets[cross$which==k, r]
    cat('K=',k,'검정데이터 \n')
    print(iris[datas_idx, ]) # 검정데이터 생성
    cat('K=',k,'훈련데이터 \n') # 학습데이터 생성
    print(iris[-datas_idx, ])
  } # outer for K
} # outer for R
