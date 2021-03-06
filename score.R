#성적표 
#상위 20% 커트라인
student= c("길동","유신","심청","감찬","성계")
#길동
subject=c("kor","eng","math","sci","soc")
score=c(75,80,92,77,90)
subject_mean=c(68.4,75.1,63.5,76.2,80.3)
subject_sd=c(4.9,11.2,9.8,5.2,8.5)
#확률면적 상위 20%지점 점수 
x <- qnorm(
  (1-0.2), #상위 20%지점 점수 
  mean = 68.4, # 구하고자 하는 과목의 평균값 
  sd=4.9 # 해당 과목의 표준편차 
)
x  # 72.52394

var.score <- data.frame(
  과목=subject <- c("kor","eng","math","sci","soc"),
  점수=score <- c(75,80,92,77,90),
  과목별평균=subject_mean <- c(68.4,75.1,63.5,76.2,80.3),
  표준편차=subject_sd <- c(4.9,11.2,9.8,5.2,8.5), ## 표준편차 
  stringsAsFactors = F
)
View(var.score)
# 상위 20% 점수를 구해서 컬럼 추가 
var.score[,"상위20"] <- round(qnorm(
(1-0.2), 
mean=var.score$과목별평균, 
sd=var.score$표준편차
),2) 
View(var.score)

# 상위 20% 패스여부 판단 
var.score[,"패스"]="실패"
var.score[var.score$점수>var.score$상위20,
          "패스"]="성공"
View(var.score)
var.score <- subset(var.score,select = -c(stringAsFacto)

# 유신의 성적표를 랜덤 데이터로 만들기 
yusin <- round(runif(5,40,100),0)
yusin

var.score_yusin <- data.frame(
  과목<- c("kor","eng","math","sci","soc"),
  점수<- c(94,65,59,86,91),
  과목별평균<- c(68.4,75.1,63.5,76.2,80.3),
  표준편차<- c(4.9,11.2,9.8,5.2,8.5),  
  stringsAsFactors = F
)
var.score_yusin[,"상위20"] <- qnorm(
  (1-0.2), 
  mean=var.score_yusin$과목별평균, 
  sd=var.score_yusin$표준편차
)
View(var.score_yusin)

var.score_yusin[,"패스"]="실패"
var.score_yusin[var.score_yusin$score>var.score_yusin$상위20,
          "패스"]="성공"
View(var.score_yusin)
