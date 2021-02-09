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
