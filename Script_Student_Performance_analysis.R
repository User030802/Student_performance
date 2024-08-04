# install.packages(tidyverse)
# library(tidyverse,dplyr,ggplot2,tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

# import the data
Student_performance_data_ <- ead_csv("students_performance_kaggle/Student_performance_data _.csv")

# Make your name shorter dont be like me :()
View(Student_performance_data_) # quick check for data format

# summarize GPA 
Student_performance_data_ %>%
  select(GPA) %>%
  summary()

# avg GPA < 2, consider factors
# Study time and Absence
# 1 Study time vs GPA
ggplot(data = Student_performance_data_, aes(x = StudyTimeWeekly, y = GPA)) +
  +     geom_point() +
  +     geom_smooth(method = "lm", se = FALSE) +
  +     labs(title = "Study Time Per Week vs. GPA",
             +          x = "Study Time Weekly (hours)",
             +          y = "GPA") +
  +     theme_minimal()
# find cor_value = cor(Student_performance_data_$StudyTimeWeekly, Student_performance_data_$GPA)

# plot for absence vs gpa
ggplot(data = Student_performance_data_ , aes(x=Absences,y=GPA)) + 
  geom_point() + geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Absence vs GPA",x = "Absences (total)", y = "GPA") + 
  theme_minimal()

correlation <- cor(Student_performance_data_$Absences, Student_performance_data_$GPA)

# comparing Parent Support to GPA
ggplot(data = Student_performance_data_, aes(x = ParentalSupport, y = GPA)) +
  +     geom_point() +
  +     labs(title = "Parental Support vs GPA",
             +          x = "Parental Support (0-4)",
             +          y = "GPA") +
  +     theme_minimal() # this graph is hard to see

# better to calculate the average 
ggplot(data = average_gpa, aes(x=factor(ParentalSupport), y = AverageGPA)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = round(AverageGPA,2)), vjust = -0.5) + 
  labs(title = "Avg GPA vs Parent Support", x ="Parent Support (0: None, 4: Very High",y = "Average GPA") + 
  theme_minimal

# tutor to GPA
GPA_avg_tutor <- Student_performance_data_ %>% group_by(Tutoring) %>% summarize(AverageGPA = mean(GPA, na.rm = TRUE))

ggplot(data = GPA_avg_tutor, aes(x=factor(Tutoring), y = AverageGPA)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(AverageGPA, 2)), vjust = -0.5) + 
  labs(title = "Avg GPA vs Tutoring", x ="Tutoring (0: No, 1: Yes", y = "Average GPA") + 
  theme_minimal()

# Extracurricular acts
# The same process as above : )
GPA_avg_extra <- Student_performance_data_ %>% group_by(Extracurricular) %>% summarize(AverageGPA = mean(GPA, na.rm = TRUE))
ggplot(data = GPA_avg_extra, aes(x=factor(Extracurricular), y = AverageGPA)) + 
  geom_bar(stat = "identity") + geom_text(aes(label = round(AverageGPA, 2)), vjust = -0.5) + 
  labs(title = "Avg GPA vs Extracurricular", x ="Extracurricular (0: No, 1: Yes)", y = "Average GPA") + 
  theme_minimal() # on Extra

GPA_avg_Sports <- Student_performance_data_ %>% group_by(Sports) %>% summarize(AverageGPA = mean(GPA, na.rm = TRUE))
ggplot(data = GPA_avg_Sports, aes(x=factor(Sports), y = AverageGPA)) + 
  geom_bar(stat = "identity") + geom_text(aes(label = round(AverageGPA, 2)), vjust = -0.5) + 
  labs(title = "Avg GPA vs Sports", x ="Sports (0: No, 1: Yes)", y = "Average GPA") + 
  theme_minimal() # on Sports

GPA_avg_Music <- Student_performance_data_ %>% group_by(Music) %>% summarize(AverageGPA = mean(GPA, na.rm = TRUE))
ggplot(data = GPA_avg_Music, aes(x=factor(Music), y = AverageGPA)) +
  geom_bar(stat = "identity") + geom_text(aes(label = round(AverageGPA, 2)), vjust = -0.5) + 
  labs(title = "Avg GPA vs Music", x ="Music (0: No, 1: Yes)", y = "Average GPA") + 
  theme_minimal() #on music

GPA_avg_Volun <- Student_performance_data_ %>% group_by(Volunteering) %>% summarize(AverageGPA = mean(GPA, na.rm = TRUE))
ggplot(data = GPA_avg_Volun, aes(x=factor(Volunteering), y = AverageGPA)) + 
  geom_bar(stat = "identity") + geom_text(aes(label = round(AverageGPA, 2)), vjust = -0.5) + 
  labs(title = "Avg GPA vs Volunter", x ="Volunter (0: No, 1: Yes)", y = "Average GPA") + 
  theme_minimal() #on volunter


