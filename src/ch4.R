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

# histogram
festivalData <- read.delim("data/DownloadFestival.dat", header = TRUE)
festivalHistogram <- ggplot(festivalData, aes(day1)) + theme(legend.position = "none")
festivalHistogram + geom_histogram()
festivalHistogram + geom_histogram( binwidth = 0.4) + labs(x = "Hygiene (Day 1 of Festival)", y = "Frequency")

festivalBoxplot <- ggplot(festivalData, aes(gender, day1))
festivalBoxplot + geom_boxplot() + labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")
tail(festivalData[order(festivalData$day1),])

# 이상치 변경
festivalData$day1[festivalData$ticknumb == 4158] <- 2.02

festivalBoxplot <- ggplot(festivalData, aes(gender, day2))
festivalBoxplot + geom_boxplot() + labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")

festivalBoxplot <- ggplot(festivalData, aes(gender, day3))
festivalBoxplot + geom_boxplot() + labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")

density <- ggplot(festivalData, aes(day1))
density + geom_density()
density + geom_density() + labs(x = "Hygiene (Day 1 of Festival)", y = "Density Estimate")

# 4.9.1.1
chickFlick <- read.delim("data/ChickFlick.dat", header = TRUE)
bar <- ggplot(chickFlick, aes(film, arousal))
bar + stat_summary(fun = mean, geom = "bar", fill = "White", colour = "Black")
bar + stat_summary(fun = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange")
bar + stat_summary(fun = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange")
bar + stat_summary(fun = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + labs( x = "Film", y = "Mean Arousal")

# 4.9.1.2
bar <- ggplot(chickFlick, aes(film, arousal, fill = gender))
bar + stat_summary(fun = mean, geom = "bar", position="dodge")
bar + stat_summary(fun = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=0.90), width=0.2) + labs( x = "Film", y = "Mean Arousal", fill = "Gender")

bar + stat_summary(fun = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=0.90), width=0.2) + labs( x = "Film", y = "Mean Arousal", fill = "Gender")  + scale_fill_manual("gender",values = c( 'Female' = "green", "Male" = "blue"))

bar <- ggplot(chickFlick, aes(film, arousal, fill = film))
bar + stat_summary(fun = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width=0.2) + facet_wrap(~ gender) + labs( x = "Film", y = "Mean Arousal") + theme(legend.position = "")

# 4.9.2 선 그래프 p197
# 4.9.2.1 독립변수 하나에 대한 선 그래프
hiccupsData <- read.delim("data/Hiccups.dat", header = TRUE)
hiccups <- stack(hiccupsData)
names(hiccups) <- c("Hiccups", "Intervention")
hiccups$Intervention_Factor <- factor(hiccups$Intervention, levels(hiccups$Intervention))
line <- ggplot(hiccups, aes(Intervention_Factor, Hiccups))
line + stat_summary(fun = mean, geom = "point")
line + stat_summary(fun = mean, geom = "line", aes(group = 1))
line + stat_summary(fun = mean, geom = "line", aes(group = 1), colour = "Blue", linetype = "dashed")
line + stat_summary(fun = mean, geom = "line", aes(group = 1), colour = "Blue", linetype = "dashed") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs( x = "Intervention", y = "Mean Number of Hiccups")

# 4.9.2.2 여러 독립변수에 대한 선 그래프
textData <- read.delim("data/TextMessages.dat", header = TRUE)
textMessages <- melt(textData, id=c("Group"), measured = c("Baseline", "Six_months"))
textMessages <- melt(textData, id=c("Group"), measured = c("Baseline", "Six_months"), variable.name = "Time", value.name = "Grammar_Score")
#names(textMessages) <- c("Group", "Time", "Grammar_Score")
#textMessages$Time <- factor(textMessages$Time, levels(textMessages$Time))
textMessages$Time <- factor(textMessages$Time, levels(textMessages$Time), labels=c("Baseline", "6_months"))
line <- ggplot(textMessages, aes(Time, Grammar_Score, colour = Group))
line + stat_summary(fun = mean, geom = 'point') + stat_summary(fun = mean, geom = "line", aes(group = Group)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs( x = "Time", y = "Mean Grammer Score", colour = "Group")
