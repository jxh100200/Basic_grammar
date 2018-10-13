# 그래프 종류 
# 데카르트 방식: plot(x,y)
# 수식기반 형식: plot(x~y)

x <- c(1:5)
x
plot(x)
plot(c(1:5))
plot(c(2,2,2,2,2)) # 하나면 x는 1씩 증가, y는 벡터의 값 
plot(1:3,3:1) ## 두개면 앞은 x, 뒤 y 
plot
plot(1:3,3:1,xlim = c(1,10),ylim = c(1,5)) ### xlim (최소값, 최대값)
plot(1:3,3:1,
     xlim = c(1,10),ylim = c(1,5),
     xlab = "x축",
     ylab = "y축",
     main = "테스트"
     )
# type="p" 점 (디폴트)
# type="l" 선, b점과선, cb에서 점생략 
# o 점과선 중첩,h점에서 x축까지 수직 
# 라인타입
# lty=0, lty="blank" 투명선
# lty=1, lty="solid" 실선 
# lty=3, lty="dotted" 점선 
# 색깔 
# col = "red"
# 배경색은 bg="yellow"
# 선의 굵기 lwd="2"
# 점이나 문자의 굵기 cex="3"

target <- round(runif(5,100,150),0)
plot(target,
     type = "o",
     col = 'red',
     ylim = c(0,200),
     axes = F,  # hide x,y축 
     ann = F # x,y 축에  no title 
     )
axis(
  1,  # x축 
  at=1:5,
  lab=c("월","화","수","목","금")
)
axis(
  2, # y축 
  ylim = c(0,200)
)
title(
  main = "과일",
  col.main="red",
  font.main=4
)
title(
  xlab = "요일",
  col.lab = "black",
)
title(
  ylab = "가격",
  col.lab = "black"
)

## multi graph
## par(mflow = c(nr,nc)) 
## nr = number of row
## nc = number of col

target <- round(runif(5,100,150),0)
par(mfrow = c(1,3))
plot(target,type = "o",  # 선+점 
     target,type = "s",  # 계단식 
     target,type = "l")  # 꺽은선 


target <- round(runif(5,100,150),0)
par(mfrow=c(1,3))
pie(target)
plot(target,type="o")
barplot(target)

par(mfrow=c(1,1))
t1 <- c(1:4)
t2 <- c(5:1)
t3 <- c(3,4,5,6,7)
plot(
  t1,type = "s", col="red",ylim = c(1,5)
)
par(new=T)
plot(
  t2,type="o",col="blue",ylim = c(1,5)
)
par(new=T)
plot(
  t3,type="l",col="green"
)



# legend 예제


par(mfrow=c(1,1))
t1 <- c(1:4)
t2 <- c(5:1)
t3 <- c(3,4,5,6,7)
plot(
  t1,type = "s", col="red",ylim = c(1,5)
)
lines(
  t2,type="o",col="blue",ylim=c(1,5)
)
lines(
  t2,type="l",col="green",ylim=c(1,5)
)
legend(
  4,     # x축의 위치 
  9,    # y축의 위치 
  c("샘플1","샘플2","샘플3"),
  cex = 0.9,   # 글자크기
  col=c("red","blue","green"),
  lty=1
)


# 막대그래프 
barplot(1:5)

#수평방향
barplot(1:5,horiz = T)

#과일판매량 막대그래프로 표현 
banana <- round(runif(5,100,150),0)
cherry <- round(runif(5,100,150),0)
orange <- round(runif(5,100,150),0)
fruit <- data.frame(
  바나나= banana,
  체리=cherry,
  오렌지=orange
)
day <- c("월","화","수","목","금")
barplot(
  as.matrix(fruit),   ### 반드시 df --> matrix로 전환해서 그려야 함 (숫자로 인식을 해야 하게함)
  main = "과일판매량",
  beside=T,
  col=rainbow(nrow(fruit)),
  ylim=c(0,400)
)
legend(
  14,400,day,cex = 0.8,
  fill = rainbow(nrow(fruit))
)

#과일판매량 막대그래프(2)로 표현 
banana <- round(runif(5,100,150),0)
cherry <- round(runif(5,100,150),0)
orange <- round(runif(5,100,150),0)
fruit <- data.frame(
  바나나= banana,
  체리=cherry,
  오렌지=orange
)
day <- c("월","화","수","목","금")
barplot(
  t(fruit),  # transform 행과 열을 바꿈 
  main = "과일판매량",
  col=rainbow(nrow(fruit)),
  ylim=c(0,600),
  space=0.1,
  cex.axis=0.8,
  las=1,
  names.arg=day,
  cex=0.8
)
legend(
  0.2,600,names(fruit),cex=0.8,
  fill=rainbow(nrow(fruit))
)

orange <- round(runif(5,100,200),0)
day <- c("월","화","수","목","금")
color <- c()
#for(){}
for (i in c(1:5)) {
  if(orange[i]>=180){
    color <- c(color,"red")
  }else if(orange[i]>=150){
    color <- c(color,"yellow")
  }else{
    color <- c(color,"green")
  }
}
barplot(
  orange, #y축 
  main = "오렌지판매량",
  col=color,
  ylim=c(0,200),
  names.arg=day  #x축 
)

if("ggplot2" %in% installed.packages("ggplot2") == FALSE)install.packages("ggplot2")
library(ggplot2)

