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
