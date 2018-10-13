if("ggplot2" %in% installed.packages("ggplot2") == FALSE)install.packages("ggplot2")
library(ggplot2)

#mpg데이터가 ggplot2 패키지 내장데이터 

ggplot(
  data = mpg,
  aes(x=displ,y=hwy)
)+geom_point()+xlim(3,6)+ylim(10,30)

library(dplyr)
## [1]건강보험심사 평가원 보건의료 빅데이터 개방시스템에서 18000여개의 관찰치와 8개의 변수가 있는 대장암 환자데이터를 통해 가장 많은 대장암이 발생하는 연령대를 
#그래프를 통해 표현하시오 
# step1 
cancer <- read.csv("https://www.dropbox.com/s/9wof8mdw7i3hto2/example_cancer.csv?dl=1")
head(cancer)
#step2
class(cancer) ## 타입이 뭐냐고 물어보는 것 
str(cancer)
#step3: 연령대별 도수(구간별 갯수)값 구하기 
degree_of_age <- table(cut(cancer$age,breaks = (1:8)*10)) ##10대, 20대 이런식으로 연령층 추출  ???
#step4:열값의 이름변경 
head(degree_of_age)
row.names(degree_of_age) <- c("10대","20대","30대","40대","50대","60대","70대")
head(degree_of_age)
#step5:시각화 
ggplot(
  data = cancer,
  aes(x=age)
)+geom_freqpoly(
    binwidth=10,size=1.4,color="red"
  )
View(degree_of_age)

## 성별 임금 소득에 대한 통계를 구하시오. 
#1. 급여에 대한 기술통계를 서술하시오 
#2. 남녀성별에 따른 평균임금을 막대그래프로 구현 
#3. 가장 고소득인 연령대를 구하시오 
#4. 그룹간 임금격차가 커지는 연령대를 구하시오 
csv="https://www.dropbox.com/s/gm4msagjec8knr1/example_salary.csv?dl=1"
salary <- read.csv(csv,
                   stringsAsFactors=T,  ###요기 잘 이해가 안됨... 
                   na="-")
salary
str(salary)
colnames(salary)  ## 컬럼명 확인하기  
# [1] "연령"                "월급여액..원."       "연간특별급여액..원."
# [4] "근로시간..시간."     "근로자수..명."       "경력구분"           
# [7] "성별" 

# step3 연산을 하기 위해 한글명을 영어로 변환 
colnames(salary) <- c("age","wage","special","working_time","worker_count","career","gender")

colnames(salary)

#step4: 검색목록에 올리기 
#salary$age를 하지 않도록 조치
#salary dataframe을 디폴트값으로 지정
## attach(salary)

##step5: 기술통계 :: 평균,중앙값, 최빈값 
# mean,meadian,mode 
salary$wage
wage_mean <- mean(salary$wage,na.rm = T)
wage_mean

# 중앙값 median
wage_mid <- median(salary$wage,na.rm = T)
wage_mid

#범위 구하기 
wage_range <- range(salary$wage,na.rm = T)
wage_range

#최고임금을 받는 사람의 정보
high_wage <- which(salary$wage==4064286)  ### 구체적인 데이터를 몰랐을 경우... 최고 연본을 어떻게 구하나 
salary[high_wage,]

# 4분위 구하기-- 전체를 4등분으로 나눠서 봄 
qnt <- quantile(salary$wage,na.rm = T)
qnt

#step6: 리스트에 담기 
sal_list <- list(
  평균월급=wage_mean,
  월급중앙값= wage_mid,
  월급범위=wage_range,
  월급사분위=qnt
)
sal_list
# 성별에 따른 임금 격차 
wage_avg_per_gender <- tapply(
  salary$wage,salary$gender,mean,na.rm=T
)
mutate(salary %>% 
      group_by(gender="여"),
      summarise(mean_wage_a=mean(salary$wage))
)
wage_avg_per_gender
# 남      여 
# 2477332 1865823

#reshape
if("reshape" %in% installed.packages("reshape2") == FALSE)install.packages("reshape2")
library(reshape2)
temp <- melt(wage_avg_per_gender)
temp
ggplot(
  data = temp,
  aes(
    x=Var1, ## 내장된 x값 
    y=value,
    fill=Var1
  )
)+geom_bar(
  stat = "identity"
)

# 커리어에 따른 임금격차 
salary$career

wage_avg_per_career <- tapply(
  salary$wage,salary$career,mean,na.rm=T
)
wage_avg_per_career

temp <- melt(wage_avg_per_career)
temp
ggplot(
  data = temp,
  aes(
    x=Var1,  
    y=value,
    fill=Var1
  )
)+geom_bar(
  stat = "identity"
)

melt <- melt(wage_avg_per_career)
ggplot(
  melt,
  aes(
    x=Var1,
    y=value,
    group=1
  )
)+geom_line(
  col="blue",
  size=2
)+coord_polar()+
  ylim(0,max(melt$value))

# 각 경력별로 제일 적게 받는 월급 집단 
#year_1 <- salary

# Var1   value
# 1  1~3년미만 1905012
# 2   10년이상 2907119
# 3    1년미만 1730835
# 4  3~5년미만 2028015
# 5 5~10년미만 2360463
tapply(
  salary$wage,
  salary$career,
  range,
  na.rm=T
)
# 
# $`1~3년미만`
# [1] 1172399 2619221
# 
# $`10년이상`
# [1] 1685204 4064286
# 
# $`1년미만`
# [1] 1117605 2414345
# 
# $`3~5년미만`
# [1] 1245540 2827420
# 
# $`5~10년미만`
# [1] 1548036 3309231

year_1 <- salary[which(salary$wage==1117605),]
year_1_3 <- salary[which(salary$wage==1172399),]
year_3_5 <- salary[which(salary$wage==1245540),]
year_5_10 <- salary[which(salary$wage==1548036),]
year_10 <- salary[which(salary$wage==1685204),]
salary$wage

career_list <- list(
  year_1,year_1_3,year_3_5,year_5_10,year_10
)
career_list

## 2번답
## 경력별로 가장 낮은 월급을 받는 집단은 대부분 60대이상 여자.
## 특이점은 10년이상 경력에서 가장 낮은 월급을 받는 집단은 20대초반 여성 
## 1886명. 이들은 10년이나 경력을 쌓고도 168만원을 수령함 

## 3번표준화 시키기 
wage_scale <- scale(salary$wage)

head(wage_scale,10)

# [,1]
# [1,] -1.28886999
# [2,] -0.91757018
# [3,] -0.38981924
# [4,] -0.06340878
# [5,]  0.37924689
# [6,]  0.31343053
# [7,]  0.28505815
# [8,] -0.04016661
# [9,] -0.13812959
# [10,] -0.78222571
## 평균이 0이고, 0을 기준으로 분산된 값들이 있다 
salary <- cbind(salary,scale=wage_scale)
str(salary)
g1 <- ggplot(salary,aes(x=salary$scale,y=salary$age))
g2 <- geom_segment(aes(yend=salary$age),xend=0)
g3 <- g1+g2+
  geom_point(
    size=7,
    aes(color=salary$gender,shape=salary$career)
  )+theme_minimal()
g3

##해석
#10년 이상된 45~54세 남성이 가장 고소득자
#25~29세 그룹은 격차가 크지 않음
#45세 이상부터는 그룹간 격차가 크다
#저임금은 주로 여성 그룹에서 나타난다
#고임금은 주로 남성그룹에서 나타난다 


# 데이터 정규화는 변숫값의 분포를 표준화하는 것을 의미한다.
# 표준화는 변수에서 데이터의 평균을 빼거나
# 변수를 전체 데이터의 표준편차로 나누는 작업을 포함한다.
# 이렇게 하면 변숫값의 평균이 0이 되고 값의
# 퍼짐정도(분포) 또한 일정해진다.
# R에서 데이터를 정규화 하는 함수는 scale()이다.
t <- c(1,2,3,4,5)
t
mean(t)
sd(t)
scale(t)






