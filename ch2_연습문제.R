Vec1 <- rep("R",5)
Vec2 <- seq(1,10,3)
Vec3 <- rep(Vec2,3)
Vec4 <- c(Vec2,Vec3)
Vec5 <- seq(25,-15,-5)
Vec6 <- Vec4[1:16]

name <- c("최민수","유관순","이순신","김유신","홍길동")
age <- c(55,45,45,53,15)
gender <- c(1,2,1,1,1)
job <- c("주부","연예인","군인","직장인","학생생")
sat <- c(3,4,2,5,5)
grade <- c("C","C","A","D","A")
total <- c(44.4,28.5,43.5,NA,27.1)

user <- data.frame(Name=name,Age=age,Gender=gender,Job=job,Sat=sat,Grade=grade,Total=total)
user
hist(user$Gender)
user2 <- user[seq(2, 4, 2), ]

kor <- c(90,95,90)
eng <- c(70,85,75)
mat <- c(86,92,88)

data<- data.frame(kor,eng,mat)
max(data$eng)
