install.packages("foreign")
library("foreign")
if("dplyr" %in% installed.packages("dplyr") == FALSE)install.packages("dplyr")
library(dplyr)
if("ggplot2" %in% installed.packages("ggplot2") == FALSE)install.packages("ggplot2")
library(ggplot2)
install.packages("readxl")
library(readxl)
raw_welfare <- read.spss(file="https://www.dropbox.com/s/w9nkhof56gzqajc/Koweps_hpc10_2015_beta1%20%281%29.sav?dl=1",to.data.frame = T)
raw_welfare
view(raw_welfare)
getwd()
setwd("C:\\Users\\Administrator\\rlang_weekend")
raw_welfare <- read.spss(file="Koweps_hpc10_2015_beta1.sav",to.data.frame = T)
View(head(raw_welfare))
head(raw_welfare)
tail(raw_welfare)
dim(raw_welfare)
str(raw_welfare)
summary(raw_welfare)

welfare <- raw_welfare

names(welfare)
welfare <- rename(welfare,
                  sex=h10_g3, # 성별 
                  birth=h10_g4, # 태여난 연도 
                  marriage=h10_g10,# 혼인 상태 
                  religion=h10_g11,# 종교 
                  income=p1002_8aq1,# 월급 
                  code_job=h10_eco9,#직업 코드 
                  code_region=h10_reg7) #지역 코드 
str(welfare)
welfare <- subset(welfare,select = c(sex,birth,marriage,religion,income,code_job,code_region))
View(head(welfare))
table(welfare$sex)

## 성별에 따른 월급차이 --> 성별에 따라 월급이 다를까? 
## 변수 검토하기 
class(welfare$sex)
table(welfare$sex)

# 이상치(outlier) 결측(NA)처리 한다 
welfare$sex <- ifelse(welfare$sex==9,NA,welfare$sex)
str(welfare)
table(is.na(welfare$sex))

welfare <- ifelse(welfare==1,"male","female")
table(welfare$sex)
qplot(welfare$sex)

## 월급 변수 검토하기 
qplot(welfare$income)+
  xlim(0,1000)

welfare$income <- ifelse(
  welfare$income%in%c(0,9999),
  NA,
  welfare$income
)
welfare$income %>%
  filter(!is.na(welfare$income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income)=mean(welfare$income)

ggplot(
  data = sex_income,
  aes(x=sex,y=mean_in)
)

## 나이와 월급의 관계 

welfare$birth <- ifelse(welfare$birth==9999,NA,welfare$birth)
table(is.na(welfare$birth))

welfare$age <- 2015- welfare$birth+1
qplot(welfare$age)
age_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income=mean(income))
age_income
ggplot(
  data=age_income,
  aes(x=age,y=mean_income))+
  geom_line()

## 파생변수 만들기: mutate 
welfare <- welfare %>% 
  mutate(ageg=ifelse(age<30,"초년",
                ifelse(age<=59,"중년","노년")))
table(welfare$ageg)
ageg_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income=mean(income))
ageg_income

sex_income <- welfare %>%  #### ageg를 찾을 수 없는 이유... 사용한 테이블이 다름 
  filter(!is.na(income)) %>% 
  group_by(sex,ageg) %>% 
  summarise(mean_income=mean(income))
sex_income
ggplot(data=sex_income,
       aes(x=ageg,
           y=mean_income,fill=sex))+
  geom_col()+
  scale_x_discrete(limits=c("초년","중년","노년"))

## 직업별 월급 차이 
class(welfare$code_job)
table(welfare$code_job)

library(readxl)
install.packages("readxl") 





