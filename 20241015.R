install.packages("reshape")
install.packages("palmerpenguins")
library(palmerpenguins)
library(reshape)
library(tidyverse)

# set working directory
setwd("C:/Users/stake/project/IPADS_statistics")

# import data
mydata <- read.delim("data/cats.dat")
mydata2 <- read.delim("data/Exam Anxiety.dat")

# define variables
name <- c("Ben", "Martin", "Andy", "Paul", "Graham", "Carina", "Karina", "Doug", "Mark", "Zoe")
DoB<-as.Date(c("1977-07-03", "1969-05-24", "1973-06-21", "1970-07-16", "1949-10-10", "1983-11-05", "1987-10-08", "1989-09-16", "1973-05-20", "1984-11-12"))
job<-c(rep(1, 5),rep(2, 5))
job<-factor(job, levels = c(1:2), labels = c("Lecturer", "Student"))
friends<-c(5,2,0,4,1,10,12,15,12, 17)
alcohol<-c(10,15,20,5,30,25,20,16,17,18)
income<-c(20000,40000,35000,22000,50000,5000,100,3000,10000,10)
neurotic<-c(10,17,14,13,21,7,13,9,14,13)

# convert to dataframe
lecturerData<-data.frame(name, DoB, job, friends, alcohol, income, neurotic)
lecturerData

#specify both rows and columns
alcoholPersonality <- lecturerData[alcohol > 10, c("friends", "alcohol", "neurotic")]

# filter data
lecturerOnly <- lecturerData[lecturerData$job == "Lecturer", ]
lecturerOnly

alcoholPersonalityMatrix <- as.matrix(alcoholPersonality)
alcoholPersonalityMatrix
alcoholPersonalityMatrix <- as.matrix(lecturerData[alcohol > 10, c("friends", "alcohol", "neurotic")])
alcoholPersonalityMatrix

satisfactionData = read.delim("data/Honeymoon Period.dat", header = TRUE)

restructuredData<-melt(satisfactionData, id = c("Person", "Gender"),
                       measured = c("Satisfaction_Base", "Satisfaction_6_Months",
                                    "Satisfaction_12_Months", "Satisfaction_18_Months"))
wideData<-cast(restructuredData, Person + Gender ~ variable, value = "value")
head(wideData)

#get penguins data
penguins

#Histograms
ggplot(penguins, aes(x = bill_length_mm)) +
  geom_histogram() +
  ggtitle("Histogram of penguin bill length ")

# Scatterplot and linear-fit
ggplot(mpg, aes(x = displ, y = hwy)) + geom_point() + labs(title ="scatterplot")
ggplot(mpg, aes(x = displ, y = hwy)) + geom_point() +
  geom_smooth(method = lm) + labs(title = "Linear Regression")

# Boxplot
ggplot(data = penguins,
       aes(x = species,
           y = bill_length_mm,
           fill = species)) + geom_boxplot() + labs(title = "Boxplot")

#scatter
examData <- read.delim("data/Exam Anxiety.dat", header = TRUE)
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + labs(x = "Exam Anxiety", y =
                        "ExamPerformance %")

#fitted line. geom_smooth(By default, the line is selected automatically.)
scatter + geom_point() + geom_smooth() + labs(x = "Exam Anxiety",
                                              y = "Exam Performance %")

#geom_smooth(method = "loess" nonparametric local weighted regression.)
scatter + geom_point() + geom_smooth(method = "loess" ) + labs(x = "Exam Anxiety", y = "Exam Performance %")

#geom_smooth(method = "lm" linear regression)
scatter + geom_point() + geom_smooth(method = "lm" ) + labs(x = "Exam Anxiety", y = "Exam Performance %")

##male female diff
scatter + geom_point(aes(colour = Gender)) +
  geom_smooth(method = "lm", aes(colour = Gender), alpha = 0.3) +
  labs(x = "Exam Anxiety", y = "Exam Performance %", colour = "Gender")

# Histgram data
festivalData <- read.delim("data/DownloadFestival.dat", header = TRUE)

# Create the plot object:
festivalHistogram <- ggplot(festivalData, aes(day1)) + theme(legend.position = "none")
# Add the graphical layer:
festivalHistogram + geom_histogram(binwidth = 0.4 ) + labs(x = "Hygiene (Day 1 of Festival)", y = "Frequency")
