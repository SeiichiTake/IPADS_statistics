library(ggplot2)
library(tidyverse)
library(psych)
library(pastecs)
library(stats)
library(car)
library(boot)
library(polycor)
setwd("C:/Users/stake/project/IPADS_statistics")

#Read the data
dlf <- read.delim("data/DownloadFestival.dat", header=TRUE)
dlf
#Remove the outlier from the day1 hygiene score
dlf$day1 <- ifelse(dlf$day1 > 20, NA, dlf$day1)

#Histogram for day 1
hist.day1 <- ggplot(dlf, aes(day1)) +
  theme(legend.position = "none") +
  geom_histogram(aes(y = after_stat(density)), colour="black", fill="white") +
  labs(x="Hygiene score on day 1", y = "Density")
hist.day1

#Histogram for day 2
hist.day2 <- ggplot(dlf, aes(day2)) +
  theme(legend.position = "none") +
  geom_histogram(aes(y = after_stat(density)) , colour="black", fill="white") +
  labs(x="Hygiene score on day 2", y = "Density")
hist.day2

#Histogram for day 3
hist.day3 <- ggplot(dlf, aes(day3)) +
  theme(legend.position = "none") +
  geom_histogram(aes(y = after_stat(density)), colour="black", fill="white") +
  labs(x="Hygiene score on day 3", y = "Density")
hist.day3

hist.day1 +
  stat_function(fun = dnorm, args = list(mean = mean(dlf$day1, na.rm = TRUE), sd = sd(dlf$day1, na.rm = TRUE)), colour = "red", size = 1)

hist.day2 + 
  stat_function(fun = dnorm, args = list(mean = mean(dlf$day2, na.rm = TRUE), sd = sd(dlf$day2, na.rm = TRUE)), colour = "red", size = 1)

hist.day3 +
  stat_function(fun = dnorm, args = list(mean = mean(dlf$day3, na.rm = TRUE), sd = sd(dlf$day3, na.rm = TRUE)), colour = "red", size = 1)

# Remove missing values and non-finite values to prevent warnings.
dlf2 <- dlf[!is.na(dlf$day1) & is.finite(dlf$day1), ]
dlf2 <- dlf[!is.na(dlf$day2) & is.finite(dlf$day2), ]
dlf2 <- dlf[!is.na(dlf$day3) & is.finite(dlf$day3), ]
imageDirectory<-("C:/Users/stake/project/IPADS_statistics/plots")

#Q-Q plot for day 1:
qqplot.day1 <- qplot(sample = dlf2$day1, geom = "qq")
qqplot.day1
ggsave(file = paste(imageDirectory,"5 Day 1 QQ.png",sep="/"))

#Q-Q plot for day 2:
qqplot.day2 <- qplot(sample = dlf2$day2, geom ="qq")
qqplot.day2
ggsave(file = paste(imageDirectory,"5 Day 2 QQ.png",sep="/"))

#Q-Q plot for day 3:
qqplot.day3 <- qplot(sample = dlf2$day3, geom ="qq")
qqplot.day3
ggsave(file = paste(imageDirectory,"5 Day 3 QQ.png",sep="/"))

#Using the describe() function for a single variable.
describe(dlf$day1)

#Two alternative ways to describe multiple variables.
describe(cbind(dlf$day1, dlf$day2, dlf$day3))
describe(dlf[,c("day1", "day2", "day3")])

#We can also use the stat.desc() function of the pastecs package.
stat.desc(dlf$day1, basic = FALSE, norm = TRUE)
stat.desc(cbind(dlf$day1, dlf$day2, dlf$day3), basic = FALSE, norm = TRUE)
round(stat.desc(dlf[, c("day1", "day2", "day3")], basic = FALSE, norm = TRUE),
      digits = 3)

#Read “RExam" data:
rexam <- read.delim("data/RExam.dat", header=TRUE)

#Set the variable uni to be a factor:
rexam$uni<-factor(rexam$uni, levels = c(0:1), labels = c("Duncetown
University", "Sussex University"))
#Check stats:
round(stat.desc(rexam[, c("exam", "computer", "lectures", "numeracy")],
                basic = FALSE, norm = TRUE), digits = 3)
#draw figure
hexam <- ggplot(rexam, aes(exam)) +
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(x = "First Year Exam Score", y = "Density") +
  stat_function(fun = dnorm, args = list(mean = mean(rexam$exam, na.rm = TRUE), sd
                                         = sd(rexam$exam, na.rm = TRUE)), colour = "red", size = 1)
hexam

#Shapiro–Wilk test
shapiro.test(rexam$exam)

#Shapiro–Wilk test by university
by(rexam$exam, rexam$uni, shapiro.test)

# Levene’s test for the exam and numeracy scores:
leveneTest(rexam$exam, rexam$uni)
leveneTest(rexam$exam, rexam$uni, center = mean)
leveneTest(rexam$numeracy, rexam$uni)

#self-help tasks
dlf$logday2 <- log(dlf$day2 + 1)
#Histogram for logday2:
hist.logday2 <- ggplot(dlf, aes(logday2)) +
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(x="Log Transformed Hygiene Score on Day 1", y = "Density") +
  stat_function(fun = dnorm, args = list(mean = mean(dlf$logday2, na.rm = TRUE), sd =
                                           sd(dlf$logday2, na.rm = TRUE)), colour = "red", size = 1)
hist.logday2

#Create square root scores
dlf$sqrtday2 <- sqrt(dlf$day2)

#Histogram for sqrtday2:
hist.sqrtday2 <- ggplot(dlf, aes(sqrtday2)) +
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(x="Square Root of Hygiene Score on Day 1", y = "Density") +
  stat_function(fun = dnorm, args = list(mean = mean(dlf$sqrtday2, na.rm = TRUE), sd = sd(dlf$sqrtday2, na.rm = TRUE)), colour = "red", size = 1)
hist.sqrtday2

#Create reciprocal scores
dlf$recday2 <- 1/(dlf$day2 + 1)


#Histogram for recday2:
hist.recday2 <- ggplot(dlf, aes(recday2)) +
  theme(legend.position = "none") +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(x="Reciprocal of of Hygiene Score on Day 1", y = "Density") +
  stat_function(fun = dnorm, args = list(mean = mean(dlf$recday2, na.rm = TRUE), sd = sd(dlf$recday2, na.rm = TRUE)), colour = "red", size = 1)
hist.recday2

stat.desc(dlf$logday2, basic = FALSE, norm = TRUE)
stat.desc(dlf$sqrtday2, basic = FALSE, norm = TRUE)
stat.desc(dlf$recday2, basic = FALSE, norm = TRUE)

#Pearson correlations (the most standard method):
examData = read.delim("data/Exam Anxiety.dat", header = TRUE)
examData

#pearson correlation
#obtain the correlation coefficient for variables other than “Gender”
cor(examData[, !names(examData) %in% "Gender"], use =
      "complete.obs", method = "pearson")

cor(examData[, !names(examData) %in% "Gender"], use =
      "complete.obs", method = "pearson")

cor(examData$Exam, examData$Anxiety, use = "complete.obs",
    method = 'pearson')

examData2 <- examData[, c("Exam", "Anxiety", "Revise")]
cor(examData2)
cor(examData[, c("Exam", "Anxiety", "Revise")])

#another way to get corr
examMatrix<-as.matrix(examData[, c("Exam", "Anxiety", "Revise")])
examMatrix

Hmisc::rcorr(examMatrix)
Hmisc::rcorr(as.matrix(examData[, c("Exam", "Anxiety", "Revise")]))

#test
cor.test(examData$Anxiety, examData$Exam)
cor.test(examData$Revise, examData$Exam)
cor.test(examData$Anxiety, examData$Revise)

cor(examData2)^2 * 100

liarData = read.delim("data/The Biggest Liar.dat", header = TRUE)
liarData

cor(liarData$Position, liarData$Creativity, method = "spearman")

cor.test(liarData$Position, liarData$Creativity, alternative = "less", method = "spearman")

cor(liarData$Position, liarData$Creativity, method = "kendall")

cor.test(liarData$Position, liarData$Creativity, alternative = "less", method = "kendall")

#If we want to bootstrap Kendall’s tau, then our function will be:
bootTau<-function(liarData,i)cor(liarData$Position[i], liarData$Creativity[i],
                                 use = "complete.obs", method = "kendall")

#To create the bootstrap object, we execute:
boot_kendall<-boot(liarData, bootTau, 5000)
boot_kendall

#To get the 95% confidence interval for the boot_kendall object:
boot.ci(boot_kendall)