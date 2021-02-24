adverts <- c(5, 4, 4, 6, 8)
packets <- c(8, 9, 10, 13, 15)
adverData <- data.frame(adverts, packets)

# 자가진단
ggplot(adverData, aes(adverts, packets)) + geom_point()

install.packages("Hmisc")
install.packages("polycor")
install.packages("boot")
install.packages("ggm")

library(boot)
library(ggm)
library(Hmisc)
library(polycor)

# 6.5.3 R로 상관분석 수행하는 일반적인 절차
examData <- read.delim("data/Exam Anxiety.dat", header = TRUE)
cor(examData, use = "complete.obs", method = "pearson") # 오류
# 피어슨
cor(examData$Exam, examData$Anxiety, use = "complete.obs", method = "pearson")
# 켄달
cor(examData$Exam, examData$Anxiety, use = "complete.obs", method = "kendall")
# 결측값 처리 방식 지정 및 쌍별 결측값 제거 사용
cor(examData$Exam, examData$Anxiety, use = "pairwise.complete", method = "kendall")

cor.test(examData$Exam, examData$Anxiety, method = "pearson")
cor.test(examData$Exam, examData$Anxiety, alternative = "less", method = "pearson")
