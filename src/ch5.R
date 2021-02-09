# P216, Self-check
festivalData <- read.delim("data/DownloadFestival(No Outlier).dat", header = TRUE)
festivalHistogram <- ggplot(festivalData, aes(day1)) + theme(legend.position = "none")
festivalHistogram + geom_histogram(aes(y = ..density..))


dlf <- read.delim("data/DownloadFestival(No Outlier).dat", header = TRUE)
hist.day1 <- ggplot( dlf, aes(day1)) + theme( legend.position = "none" ) + geom_histogram( aes( y = ..density..), colour = "black", fill = "white" ) + labs( x = "Hygiene score on day 1", y = "Density" )
hist.day1

hist.day1 + stat_function(fun = dnorm, args = list( mean = mean(dlf$day1, na.rm = TRUE), sd = sd(dlf$day1, na.rm = TRUE)), colour = "black", size = 1)
ggplot(dlf, aes(sample=dlf$day1)) + stat_qq()
hist.day1


hist.day2 <- ggplot( dlf, aes(day2)) + theme( legend.position = "none" ) + geom_histogram( aes( y = ..density..), colour = "black", fill = "white" ) + labs( x = "Hygiene score on day 2", y = "Density" )
hist.day2 + stat_function(fun = dnorm, args = list( mean = mean(dlf$day2, na.rm = TRUE), sd = sd(dlf$day2, na.rm = TRUE)), colour = "black", size = 1)
ggplot(dlf, aes(sample=dlf$day2)) + stat_qq()

hist.day3 <- ggplot( dlf, aes(day3)) + theme( legend.position = "none" ) + geom_histogram( aes( y = ..density..), colour = "black", fill = "white" ) + labs( x = "Hygiene score on day 3", y = "Density" )
hist.day3 + stat_function(fun = dnorm, args = list( mean = mean(dlf$day3, na.rm = TRUE), sd = sd(dlf$day3, na.rm = TRUE)), colour = "black", size = 1)
ggplot(dlf, aes(sample=dlf$day3)) + stat_qq()

# 5.5.2 정규성의 수량화
library(psych)
describe(dlf$day1)

library(pastecs)
stat.desc(dlf$day1, basic = TRUE, norm = FALSE)
stat.desc(dlf$day1, basic = FALSE, norm = TRUE)

describe(cbind(dlf$day1, dlf$day2, dlf$day3))
stat.desc(cbind(dlf$day1, dlf$day2, dlf$day3), basic = FALSE, norm = TRUE)

describe(dlf[, c("day1", "day2", "day3")])
stat.desc(dlf[, c("day1", "day2", "day3")], basic = FALSE, norm = TRUE)
round(stat.desc(dlf[, c("day1", "day2", "day3")], basic = FALSE, norm = TRUE), digits = 3)

# 5.5.3 자료 그룹들을 다루기
# 
# 5.5.3.1 자료 전체의 분석석
rexam <- read.delim("data/RExam.dat", header = TRUE)
rexam$uni <- factor(rexam$uni, levels = c(0:1), labels = c("Duncetown University", "Sussex University"))
describe(rexam[, c("exam", "computer", "lectures", "numeracy")])
round(stat.desc(rexam[, c("exam", "computer", "lectures", "numeracy")], basic = FALSE, norm = TRUE), 3)

hist.exam <- ggplot( rexam, aes(exam)) + theme( legend.position = "none" ) + geom_histogram( aes( y = ..density..), colour = "black", fill = "white" ) + labs( x = "First Year Exam Score", y = "Density" )
hist.exam + stat_function(fun = dnorm, args = list( mean = mean(rexam$exam, na.rm = TRUE), sd = sd(rexam$exam, na.rm = TRUE)), colour = "black", size = 1)

hist.computer <- ggplot( rexam, aes(computer)) + theme( legend.position = "none" ) + geom_histogram( aes( y = ..density..), colour = "black", fill = "white" ) + labs( x = "Computer Literacy", y = "Density" )
hist.computer + stat_function(fun = dnorm, args = list( mean = mean(rexam$computer, na.rm = TRUE), sd = sd(rexam$computer, na.rm = TRUE)), colour = "black", size = 1)

hist.lectures <- ggplot( rexam, aes(lectures)) + theme( legend.position = "none" ) + geom_histogram( aes( y = ..density..), colour = "black", fill = "white" ) + labs( x = "Percentage of Lectures Attended", y = "Density" )
hist.lectures + stat_function(fun = dnorm, args = list( mean = mean(rexam$lectures, na.rm = TRUE), sd = sd(rexam$lectures, na.rm = TRUE)), colour = "black", size = 1)

hist.numeracy <- ggplot( rexam, aes(numeracy)) + theme( legend.position = "none" ) + geom_histogram( aes( y = ..density..), colour = "black", fill = "white" ) + labs( x = "Numeracy", y = "Density" )
hist.numeracy + stat_function(fun = dnorm, args = list( mean = mean(rexam$numeracy, na.rm = TRUE), sd = sd(rexam$numeracy, na.rm = TRUE)), colour = "black", size = 1)

# 5.5.3.2 그룹별 분석
by(data = rexam$exam, INDICES = rexam$uni, FUN = describe)
by(data = rexam$exam, INDICES = rexam$uni, FUN = stat.desc, basic = FALSE, norm = TRUE)
by(data = rexam$exam, rexam$uni, describe)
by(data = rexam$exam, rexam$uni, stat.desc)
by(cbind(data = rexam$exam, data = rexam$numeracy), rexam$uni, describe)
by(cbind(rexam$exam, rexam$numeracy), rexam$uni, describe)
by(rexam[, c("exam", "numeracy")], rexam$uni, describe)

dunceData <- subset(rexam, rexam$uni=="Duncetown University")
sussexData <- subset(rexam, rexam$uni=="Sussex University")

hist.numeracy.duncetown <- ggplot(dunceData, aes(numeracy)) + theme(legend.position = "none") + geom_histogram(aes(y = ..density..), fill = "white", colour = "black", binwidth = 1) + labs(x = "Numeracy Score", y = "Density") + stat_function(fun = dnorm, args=list(mean = mean(dunceData$numeracy, na.rm = TRUE), sd = sd(dunceData$numeracy, na.rm = TRUE)), colour = "blue", size = 1)
hist.numeracy.duncetown

hist.numeracy.sussextown <- ggplot(sussexData, aes(numeracy)) + theme(legend.position = "none") + geom_histogram(aes(y = ..density..), fill = "white", colour = "black", binwidth = 1) + labs(x = "Numeracy Score", y = "Density") + stat_function(fun = dnorm, args=list(mean = mean(sussexData$numeracy, na.rm = TRUE), sd = sd(sussexData$numeracy, na.rm = TRUE)), colour = "blue", size = 1)
hist.numeracy.sussextown

hist.exam.duncetown <- ggplot(dunceData, aes(exam)) + theme(legend.position = "none") + geom_histogram(aes(y = ..density..), fill = "white", colour = "black", binwidth = 1) + labs(x = "Exam Score", y = "Density") + stat_function(fun = dnorm, args=list(mean = mean(dunceData$exam, na.rm = TRUE), sd = sd(dunceData$exam, na.rm = TRUE)), colour = "blue", size = 1)
hist.exam.duncetown

hist.exam.sussextown <- ggplot(sussexData, aes(exam)) + theme(legend.position = "none") + geom_histogram(aes(y = ..density..), fill = "white", colour = "black", binwidth = 1) + labs(x = "Exam Score", y = "Density") + stat_function(fun = dnorm, args=list(mean = mean(sussexData$exam, na.rm = TRUE), sd = sd(sussexData$exam, na.rm = TRUE)), colour = "blue", size = 1)
hist.exam.sussextown

hist.computer.duncetown <- ggplot(dunceData, aes(computer)) + theme(legend.position = "none") + geom_histogram(aes(y = ..density..), fill = "white", colour = "black", binwidth = 1) + labs(x = "Computer Score", y = "Density") + stat_function(fun = dnorm, args=list(mean = mean(dunceData$computer, na.rm = TRUE), sd = sd(dunceData$computer, na.rm = TRUE)), colour = "blue", size = 1)
hist.computer.duncetown

hist.computer.sussextown <- ggplot(sussexData, aes(computer)) + theme(legend.position = "none") + geom_histogram(aes(y = ..density..), fill = "white", colour = "black", binwidth = 1) + labs(x = "Computer Score", y = "Density") + stat_function(fun = dnorm, args=list(mean = mean(sussexData$computer, na.rm = TRUE), sd = sd(sussexData$computer, na.rm = TRUE)), colour = "blue", size = 1)
hist.numeracy.sussextown

# 5.6.1 R에서 샤피로-윌크 검정 실행하기
shapiro.test(rexam$exam)
shapiro.test(rexam$numeracy)
by(rexam$exam, rexam$uni, shapiro.test)
by(rexam$numeracy, rexam$uni, shapiro.test)
# Q-Q Plot
ggplot(rexam, aes(sample=rexam$exam)) + stat_qq()
ggplot(rexam, aes(sample=rexam$numeracy)) + stat_qq()

# 5.7 분산의 동질성 검정
# 5.7.1.2 R을 이용한 레빈 검정 실행
install.package("car")
library(car)
leveneTest(rexam$exam, rexam$uni)
leveneTest(rexam$numeracy, rexam$uni)
