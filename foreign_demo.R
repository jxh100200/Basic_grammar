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
raw_welfare <- read.spss(file="Koweps_hpc10_2015_beta1 .sav",to.data.frame = T)
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
welfare <- subset(welfare,select = c(sex,birth,marriage,religion,income,code_job,code_region))
View(head(welfare))


## 성별에 따른 월급차이 --> 성별에 따라 월급이 다를까? 
## 변수 검토하기 
class(welfare$sex)
table(welfare$sex)

# 이상치(outlier) 결측(NA)처리 한다 
welfare <- ifelse(welfare$sex==9,NA,welfare$sex)
table(is.na(welfare$sex))

welfare$sex <- ifelse(welfare==1,"male","female")
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
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income)=mean(income)

ggplot(
  data = sex_income
)










  
