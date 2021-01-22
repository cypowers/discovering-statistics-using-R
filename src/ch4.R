library(ggplot2)
examData <- read.delim("data/Exam Anxiety.dat", header = TRUE)
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point()
scatter + geom_point() + labs(x = "Exam Anxiety", y = "Exam Performance %")
scatter + geom_point() + geom_smooth() + labs(x = "Exam Anxiety", y = "Exam Performance")
scatter + geom_point() + geom_smooth(method = "lm") + labs(x = "Exam Anxiety", y = "Exam Performance")
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red") + labs(x = "Exam Anxiety", y = "Exam Performance")
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se=F) + labs(x = "Exam Anxiety", y = "Exam Performance")
scatter + geom_point() + geom_smooth(method = "lm", alpha = 0.1, fill = "Blue") + labs(x = "Exam Anxiety", y = "Exam Performance")


# 22 Jan 2021
scatter <- ggplot(examData, aes(Anxiety, Exam, colour = Gender))
scatter + geom_point() + geom_smooth(method = 'lm')
scatter + geom_point() + geom_smooth(method = 'lm', aes(fill = Gender), alpha = 0.1)
scatter + geom_point() + geom_smooth(method = 'lm', aes(fill = Gender), alpha = 0.1) + labs(x = 'Exam Anxiety', y = 'Exam Performance %', colour = 'Gender')
