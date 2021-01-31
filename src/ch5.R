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
