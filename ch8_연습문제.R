# 1) 다음 조건에 맞는 quakes 데이터 셋의 수심(depth)과 리히터 규모(mag)가 동일한 패널에 지진의 발생지를 산점도로 시각화 하시오

# 수심 3개영역으로 범주화
range(quakes$depth)
quakes$depth4[quakes$depth >= 40 & quakes$depth <= 250] <- 1
quakes$depth4[quakes$depth >= 251 & quakes$depth <= 450] <- 2
quakes$depth4[quakes$depth >= 451 & quakes$depth <= 680] <- 3

deptgroup2 <- equal.count(quakes$depth, number = 3, overlap=0)
deptgroup2


.# 리히터 규모 2개 영역으로 범주화
head(quakes)
range(quakes$mag)
quakes$mag4[quakes$mag >= 4.0 & quakes$mag <= 5.2]

maggroup<- equal.count(quakes$mag, number=2,overlap=0)
maggroup

# 수심과 리히터 규모가 3행 2열 구조의 패널로 산점도 그래프 그리기
xyplot(lat~long|deptgroup2*maggroup,data=quakes,ylab="위도",xlab ="경도", pch='@',col=c("red","blue"))


# 2) latticeExtra 패키지에서 제공되는 SeatacWeather 데이터 셋에서 월별로 최저 기온과 최고기온을 선 그래프로 플로팅 하시오.

# lattice 패키지의 xyplot()함수 이용
# 선그래프 type = "l"

install.packages("latticeExtra")
library(latticeExtra)
data(SeatacWeather)
str(SeatacWeather)
head(SeatacWeather)
summary(SeatacWeather)

xyplot(max.temp+min.temp~day|month,data=SeatacWeather,type="l",auto.key = T,layout=c(3,1))

#3 diamonds 데이터 셋을 대상으로 x축에 carat 변수, y축에 price 변수를 지정하고, clarity변수를 선 색으로 지정하여 미적 요소 맵핑 객체를 생성한 후 산점도 그래프 주번에 부드러운 곡선이 추가되도록 레이아웃을 추가하시오.


pw<- ggplot(diamonds,aes(x=carat,y=price,color = clarity))
pw + geom_point() + geom_smooth()

















