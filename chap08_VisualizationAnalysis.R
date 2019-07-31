# chap08_VisualizationAnalysis

####################################
## Chapter08. 고급 시각화 분석 #####
####################################


## 1. R 고급시각화 도구
# 제공 패키지 - graphics/lattice/ ggplot2/ ggmap 등

## 2. 격자형 기법 시각화
install.packages("lattice")
library(lattice)
help("equal.count")

install.packages("mlmRev")
library(mlmRev)
data(Chem97)

str(Chem97)
# data.frame':	31022 obs. of  8 variables:

table(Chem97$score)
range(Chem97$gcsescore) # 0~8

# 2.1 히스토그램
#  형식) histogram(~x축 컬럼| 조건, data... )

histogram(~gcsescore, data=Chem97)
# gcsescore 변수를 대상으로 백분율 적용 히스토그램.
hist(Chem97$gcsescore)

table(Chem97$score)

histogram(~gcsescore|score, data = Chem97)
# score 단위
histogram(~gcsescore|factor(score), data = Chem97)
# score 요인 단위

View(Chem97)

# 2.2 밀도그래프 
# - 형식 densityplot(~x축 | 조건, data=df, groups=변수)
# - 성별번수를 그룹으로 지정하여 GCSE점수를 score 단위로 밀도그래프를 플로팅
densityplot(~gcsescore|factor(score),data = Chem97, groups = gender, auto.key = T, plot.points=F)

# 2.3 막대그래프
# - 형식) barchart(y축컬럼 ~ x축컬럼| 조건, data, layout)

# 1) 데이터셋 가져오기
data("VADeaths")
str(VADeaths)
# num [1:5, 1:4] 11.7 18.1 26.9 41 66 8.7 11.7 20.3 30.9 54.3 ...

# 2) 데이터셋 구조보기
mode(VADeaths) # numeric
class(VADeaths) # matrix

table(VADeaths)

# 3) 데이터 형식 변경(matrix 형식을 dataframe 형식으로 변경)
# (1) matrix -> dataframe 변환
df<-as.data.frame(VADeaths)
class(df)
# "data.frame"
str(df)
# 'data.frame':	5 obs. of  4 variables:

# (2) matrix -> table 변환
dft <- as.data.frame.table(VADeaths)
str(dft)
# 'data.frame':	20 obs. of  3 variables:
class(dft)
# "data.frame"
dft # Var1 Var2 Freq -> 1열을 기준으로 data.table 생성

# 막대 그래프 그리기
barchart(Var1 ~ Freq|Var2, data=dft, layout=c(4,1), origin=0)
# origin 속성 : x축의 구간을 0부터 표시해주는 역할

# 2.4 점 그래프
# - 형식) dotplot(y축컬럼 ~ x축컬럼|조건,data,layout)
dotplot(Var1~Freq|Var2, dft)  # layout속성 생략시 2행2열을 기본값으로 가짐     

dotplot(Var1~Freq|Var2, dft,layout=c(4,1))

# Var2 변수 단위로 그룹화하여 점을 연결하여 플로팅
dotplot(Var1~Freq, data=dft, groups=Var2, type = "o",auto.key=list(space="right",points=T,lines=T))

# 2.5 산점도 그래프
# - 형식) xyplot(y축컬럼 ~ x축컬럼|조건,data=data.frame or list, layout)
str(airquality)
# 'data.frame':	153 obs. of  6 variables:
head(airquality)
# OZONE SOLAR.R WIND TEMP MONTH DAY

# airquality의 OZONE(y), WIND(x) 산점도 플로팅
names(airquality) <- c('Ozone','Solar.R','Wind','Temp','Month','Day')

xyplot(Ozone~Wind, data=airquality)
range(airquality$Ozone, na.rm = T) #1~168
xyplot(Ozone~Wind|factor(Month), data=airquality)
xyplot(Ozone~Wind|factor(Month), data=airquality,layout=c(5,1))


# airquality 데이터셋의 Month 타입 factor형 변경
convert <- transform(airquality, Month=factor(Month))
str(convert) #'data.frame':	153 obs. of  6 variables:
              #  $ Month  : Factor
head(convert) #  Ozone Solar.R Wind Temp Month Day

xyplot(Ozone~Wind|Month, data=convert)


# quakes 데이터 셋으로 산점도 그래프 그리기
View(quakes)
str(quakes) #'data.frame':	1000 obs. of  5 variables:
range(quakes$stations) # 10~132

# 지진 발생 위치(위도와 경도)
xyplot(lat~long, data= quakes, pch="o")

# 그래프를 변수에 저장
tplot <- xyplot(lat~long, data= quakes, pch=".")

# 그래프에 제목 추가
tplot2 <- update(tplot, main= "1964년 이후 태평야에서 발생한 지진위치치")

tplot2

#산점도 그래프 그리기
# 1. depth 이산형 변수 범위 확인
range(quakes$depth) # 40~680

# 2. depth 변수리코딩 : 6개의 범주(100단위)로 코딩변경
quakes$depth2[quakes$depth >= 40 & quakes$depth <= 150] <- 1
quakes$depth2[quakes$depth >= 151 & quakes$depth <= 250] <- 2
quakes$depth2[quakes$depth >= 251 & quakes$depth <= 350] <- 3
quakes$depth2[quakes$depth >= 351 & quakes$depth <= 450] <- 4
quakes$depth2[quakes$depth >= 451 & quakes$depth <= 550] <- 5
quakes$depth2[quakes$depth >= 551 & quakes$depth <= 680] <- 6

# 3. 리코딩 변수(depth2)를 조건으로 산점도 그래프 그리기
convert <- transform(quakes, depth2=factor(depth2))
xyplot(lat~long|depth2, data=convert)

# 동일한 패널에 2개의 y축에 값을 포현
xyplot(Ozone+Solar.R~Wind|factor(Month),data = airquality,auto.key = T)


# 2.6 데이터 범주화

# equal.count() 함수를 이용 이산형 변수 범주화

# (1) 1~150을 대상으로 겹치지 않게 4개 영역으로 범주화
numgroup<-equal.count(1:150,number=4,overlap=0)

# (2) 지진의 깊이를 6개 영역으로 범주화 
depthgroup <- equal.count(quakes$depth,number=6,overlap=0)
depthgroup

# 범주화된 변수(depthgroup)를 조건으로 산점도 그래프 그리기
xyplot(lat~long|depthgroup, data = quakes, main = "Fiji Earthquakes", ylab = 'latitude', xlab = '경도', col="red",pch = "+")

# 수심과 리터규모 변수를 동시에 적용하여 산점도 그래프 그리그
magnitudegroup <- equal.count(quakes$mag,number=2, overlap=0)
magnitudegroup

# magnitudegroup  변수 기준으로 플로팅
xyplot(lat~long|magnitudegroup, data=quakes, main="Piji Earthquakes", ylab = "위도", xlab='경도', pch="@")

# 수심과 리터규모를 동시에 표현
xyplot(lat~long|depthgroup*magnitudegroup, data=quakes, main="fiji Earthquakes", ylab="위도", xlab="경도", pch='+',col=c('red','blue'),auto.key=T)

# 이산형 변수로 리코딩한 뒤에 factor형으로 변환하여 산점도 그래프 그리기

# depth 변수 리코딩분
quakes$depth3[quakes$depth >= 39.5 & quakes$depth <= 80.5 ] <- 'd1'
quakes$depth3[quakes$depth >= 79.5 & quakes$depth <= 186.5 ] <- 'd2'
quakes$depth3[quakes$depth >= 185.5 & quakes$depth <= 397.5 ] <- 'd3'
quakes$depth3[quakes$depth >= 396.5 & quakes$depth <= 562.5 ] <- 'd4'
quakes$depth3[quakes$depth >= 562.5 & quakes$depth <= 680.5 ] <- 'd5'

str(quakes)

quakes$mag3[quakes$mag >= 3.95 & quakes$mag <= 4.65] <- 'm1'
quakes$mag3[quakes$mag >= 4.55 & quakes$mag <= 6.45] <- 'm2'

convert <- transform(quakes, depth3=factor(depth3),mag3=factor(mag3))
str(convert)

xyplot(lat~long|depth3*mag3, data=convert, main="fiji Earthquakes", ylab="위도", xlab="경도", pch='+',col=c('red','blue'))

# 2.7 조건 그래프
coplot(lat~long|depth, data = quakes)
coplot(lat~long|depth, data = quakes, overlap = 0.1)
coplot(lat~long|depth, data = quakes, number= 5)
coplot(lat~long|depth, data = quakes, number = 5,row = 1)

# 패널과 조건 막대에 색 적용 후 조건 그래프 그리기
coplot(lat~long|depth, data= quakes, number=5, row=1, panel=panel.smooth)

coplot(lat~long|depth, data= quakes, number=5, row=1, col="blue",bar.bg = c(num="green")) # 패널과 조건막대 색

# 2.8 3차원 산점도 그래프

# 위도, 경도, 깊이를 이용하여 3차원 산점도 그래프 그리기
cloud(depth~lat*long,data=quakes, zlim=rev(range(quakes$depth)),xlab = "경도",ylab = "위도",zlab="수심")

# 테두리와 회전 속성을 추가
cloud(depth~lat*long,data=quakes, zlim=rev(range(quakes$depth)),xlab = "경도",ylab = "위도",zlab="수심", panel.aspect=0.9, screen=list(z=45,x=-25))

# 3. 기하학적 기법 시각화(ggplot2 package)
# 3.1 qplot() 함수

install.packages('ggplot2')
library(ggplot2)

data(mpg)
View(mpg)

str(mpg)
head(mpg)
summary(mpg)
table(mpg$drv)

# (1) 한개 변수 대상 qplot() 함수적용
help("qplot")
qplot(data=mpg,x=hwy) #세로 막대 그래프

# fill 속성 : hwy 변수를 대상으로 drv 변수에 색채우기(누적 막대 그래프)
qplot(data=mpg,x=hwy,fill=drv) #세로 막대 그래프

# binwidth 속성: 막대 폭 지정 옵션
qplot(data=mpg, x=hwy, fill=drv, binwidth=10)

# facets 속성: drv 변수 값으로 컬럼단위와 행단위로 패널 생성.

# 열단위 패널생성
qplot(data=mpg, x=hwy, fill=drv, binwidth=2,facets = .~drv) 
# 행단위 패널생성
qplot(data=mpg, x=hwy, fill=drv, binwidth=1,facets = drv~.)

# (2) 두개 변수 대상 qplot() 함수 적용.
qplot(displ,hwy, data=mpg) # mpg 데이터셋의 displ 과  hwy변수 이용 (산점도).

# displ, hwy 변수 대상으로 drv 변수값으로 색상 적용 산점도 그래프.
qplot(displ,hwy, data=mpg, color=drv)

# displ 과 hwy 변수와의 관계를 drv로 구분.
qplot(displ,hwy, data=mpg, color=drv, facets= .~drv)

# (3) 미적 요소 맵핑(mapping)
View(mtcars)
str(mtcars) # 'data.frame':	32 obs. of  11 variables:

#색상적용 카뷰레이터의 수
qplot(wt,mpg,data=mtcars,color=factor(carb))
# 크기적용
qplot(wt,mpg,data=mtcars,color=factor(carb),size=qsec)
#모양적용
qplot(wt,mpg,data=mtcars,color=factor(carb),size=qsec, shape=factor(cyl))

# (4) 기하학적 객체 적용

View(diamonds)
str(diamonds)
# ‘tbl_df’, ‘tbl’ and 'data.frame':	53940 obs. of  10 variables:

# geom="bar" 속성으로 막대그래프 그리기
# -> clarity 변수 대상 cut 변수로 색 채우기
qplot(clarity,data=diamonds,fill=cut,geom="bar")

# geom="point"
qplot(wt,mpg,data=mtcars,size=qsec,geom = "point")

# cyl 변수의 요인으로 point 크기 적용, carb 변수의 요인으로 포인트 색 적용

qplot(wt,mpg,data = mtcars, size=factor(cyl),geom = "point", color = factor(carb))

# qsec 변수로 포인트 크기 적용, cyl 변수의 요인으로 point 모양 적용
qplot(wt,mpg,data=mtcars,size=qsec,color=factor(carb),shape=factor(cyl),geom="point")

# geom = "smooth"
# cyl변수 요인으로 색상 적용
qplot(wt, mpg, data=mtcars, color=factor(cyl), geom=c("point","smooth"))

# geom="line"
qplot(mpg,wt,data = mtcars,color=factor(cyl),geom="line")

# 3.2 ggplot() 함수

# 단계1(layer1) : 배경 설정하기.

# x축은 dipl, y축은 hwy로 지정해 배경 생성
x <- ggplot(mpg, aes(x=displ,y=hwy)) # aesthetics 미학
x
# 단계2(layer2) : 그래프 추가하기
# 배경에 산점도 추가
y <- x+geom_point()
y

# 단계3(layer3) : 축범위를 조정하는 설정
z <- y + xlim(3,6) + ylim(10,30)
z

# (1) 미적 요소 맵핑
p<- ggplot(diamonds, aes(carat,price,color=cut))
p + geom_point()

# (2) 기하학적 객체(geometric object:점/선/도형/막대) 적용
p <- ggplot(mtcars, aes(mpg,wt,color=factor(cyl)))
p + geom_line()

p <- ggplot(mtcars, aes(mpg,wt,color=factor(cyl)))
p + geom_point()

# (3) 미적 요소 맵핑과 기하학적 객체 적용
p <- ggplot(diamonds, aes(price))
p + stat_bin(aes(fill=cut), geom="bar")
p + stat_bin(aes(fill=..density..), geom="bar")

p <- ggplot(diamonds, aes(price))
p + stat_bin(aes(fill=cut), geom="area")
p + stat_bin(aes(fill=..density..), geom="point")

# (4) 테마(theme) 적용
p <- ggplot(diamonds, aes(carat,price,color = cut))
p<-p + geom_point() + ggtitle("다이아몬드 무게와 가격의 상관관계")

p<- p + theme(
  title = element_text(color = "blue", size = 25), # 축제목
  axis.title=element_text(size = 14, face="bold"), # 축제목
  axis.title.x = element_text(color="green"), # x축제목
  axis.title.y=element_text(color = "red"), # y축제목
  axis.text = element_text(size = 14), # 축이름 크기기   
  axis.text.x = element_text(color="orange"), # x 축이름
  axis.text.y=element_text(color = "blue"), # 축 이름
  legend.title=element_text(size=20,face="bold",color = "red"), #범례
  legend.position="bottom",
  legend.direction="horizontal"
  )
p

# 3.3 ggsave() 함수

ggplot(diamonds, aes(carat,price,color=cut)) + geom_point()

ggsave(file="C:/workspaces/R/output/diamond_price.pdf")

ggsave(file="C:/workspaces/R/output/diamond_price.jpg", dpi = 300 )

# 변수에 저장된 그래프
ggsave(file="C:/workspaces/R/output/test.png",plot=p,width = 10,height = 5 )

# ggmap  ㅈ
install.packages("ggplot2")
install.packages("ggmap")
remove.packages("ggplot2")
remove.packages("ggmap")


library(ggplot2)
library(ggmap)
register_google(key='AIzaSyAXXqXEGAnqdifFEBzpIWdK3bzA0SRmwH8 ')

gc <- geocode("seoul")

center <- as.numeric(gc)
center

map <- get_googlemap(center=center, language = "ko-KR", color="bw",scale=2)

map





























