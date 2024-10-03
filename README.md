Project Title: Analyze student academic performance and factors that can affect 

Dataset: https://github.com/User030802/Student_performance/blob/main/Student_performance_data%20_.csv

Findings: Students are encouraged to participate in activities as well as limiting their absenteeism. Also
parental support is necessary to their student GPA performance

# 1. Importing Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

- Data importing
Student_performance_data_ <- ead_csv("students_performance_kaggle/Student_performance_data _.csv")

# Make your name shorter dont be like me :()
View(Student_performance_data_) # quick check for data format

summarize GPA 
Student_performance_data_ %>%
  select(GPA) %>%
  summary()

<img width="134" alt="image" src="https://github.com/user-attachments/assets/e950cea9-d3f3-400a-b70e-59dd6fb06d4c">

# 2. Data Analysis
avg GPA < 2, consider factors
Study time and Absence
# 1 Study time vs GPA
ggplot(data = Student_performance_data_, aes(x = StudyTimeWeekly, y = GPA)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Study Time per Week vs GPA", 
       x = "Study Time Weekly (Hours)", 
       y = "GPA") +
  theme_minimal()

<img width="553" alt="image" src="https://github.com/user-attachments/assets/71d3ac2d-f50f-4ae2-9735-40c99ca8fa44">

# 2 Correlation
correlation_value <- cor(Student_performance_data_$StudyTimeWeekly, Student_performance_data_$GPA)
> sprintf("Correlation is %f", correlation_value)
[1] "Correlation is 0.179275"

# 3 Absence vs GPA
ggplot(data = Student_performance_data_ , aes(x=Absences,y=GPA)) 
+ geom_point() + geom_smooth(method = "lm", se = FALSE) +
+ labs(title = "Absence vs GPA",x = "Absences (total)", y = "GPA") + theme_minimal()

<img width="548" alt="image" src="https://github.com/user-attachments/assets/36d9539f-251c-4360-b162-cf7b0ba80a8e">

# 4 Ran same correlation as bove

# 5 Parental support vs GPA 
ggplot(data = Student_performance_data_, aes(x = ParentalSupport, y = GPA)) +
  geom_point() +
  labs(title = "Parental Support vs GPA",
       x = "Parental Support (0-4)",
       y = "GPA") +
  theme_minimal()

<img width="544" alt="image" src="https://github.com/user-attachments/assets/10608995-41f2-42ef-9168-0a54e6147e62">

# 6 Averages
ggplot(data = average_gpa, aes(x = factor(ParentalSupport), y = AverageGPA)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = round(AverageGPA, 2)), vjust = -0.5) + 
  labs(title = "Avg GPA vs Parent Support", 
       x = "Parent Support (0: None, 4: Very High)", 
       y = "Average GPA") + 
  theme_minimal()

<img width="306" alt="image" src="https://github.com/user-attachments/assets/e5f20469-840e-4d85-95b9-21d6046e4f76">

# 7 Tutor to GPA
GPA_avg_tutor <- Student_performance_data_ %>% group_by(Tutoring) %>% summarize(AverageGPA = mean(GPA, na.rm = TRUE))

ggplot(data = GPA_avg_tutor, aes(x=factor(Tutoring), y = AverageGPA)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(AverageGPA, 2)), vjust = -0.5) + 
  labs(title = "Avg GPA vs Tutoring", x ="Tutoring (0: No, 1: Yes", y = "Average GPA") + 
  theme_minimal()

<img width="307" alt="image" src="https://github.com/user-attachments/assets/a2a5c870-03d1-4474-b37e-389e3ff5bdc8">


# 8 Extracurricular Act
GPA_avg_extra <- Student_performance_data_ %>% group_by(Extracurricular) %>% summarize(AverageGPA = mean(GPA, na.rm = TRUE))

ggplot(data = GPA_avg_extra, aes(x=factor(Extracurricular), y = AverageGPA)) + 
  geom_bar(stat = "identity") + geom_text(aes(label = round(AverageGPA, 2)), vjust = -0.5) + 
  labs(title = "Avg GPA vs Extracurricular", x ="Extracurricular (0: No, 1: Yes)", y = "Average GPA") + 
  theme_minimal() # on Extra

<img width="303" alt="image" src="https://github.com/user-attachments/assets/b5da9940-ffea-4436-a677-40db500b2ce9">


GPA_avg_Sports <- Student_performance_data_ %>% group_by(Sports) %>% summarize(AverageGPA = mean(GPA, na.rm = TRUE))

ggplot(data = GPA_avg_Sports, aes(x=factor(Sports), y = AverageGPA)) + 
  geom_bar(stat = "identity") + geom_text(aes(label = round(AverageGPA, 2)), vjust = -0.5) + 
  labs(title = "Avg GPA vs Sports", x ="Sports (0: No, 1: Yes)", y = "Average GPA") + 
  theme_minimal() # on Sports

<img width="306" alt="image" src="https://github.com/user-attachments/assets/ea72abb4-3b8f-4df2-ac55-a6a2ccbae31f">




GPA_avg_Music <- Student_performance_data_ %>% group_by(Music) %>% summarize(AverageGPA = mean(GPA, na.rm = TRUE))

ggplot(data = GPA_avg_Music, aes(x=factor(Music), y = AverageGPA)) +
  geom_bar(stat = "identity") + geom_text(aes(label = round(AverageGPA, 2)), vjust = -0.5) + 
  labs(title = "Avg GPA vs Music", x ="Music (0: No, 1: Yes)", y = "Average GPA") + 
  theme_minimal() #on music

<img width="302" alt="image" src="https://github.com/user-attachments/assets/c4664609-8683-4b85-a8d7-0108e27dbb66">


GPA_avg_Volun <- Student_performance_data_ %>% group_by(Volunteering) %>% summarize(AverageGPA = mean(GPA, na.rm = TRUE))

ggplot(data = GPA_avg_Volun, aes(x=factor(Volunteering), y = AverageGPA)) + 
  geom_bar(stat = "identity") + geom_text(aes(label = round(AverageGPA, 2)), vjust = -0.5) + 
  labs(title = "Avg GPA vs Volunter", x ="Volunter (0: No, 1: Yes)", y = "Average GPA") + 
  theme_minimal() #on volunter

<img width="303" alt="image" src="https://github.com/user-attachments/assets/0c57e115-04e6-408e-8929-674d0184a2f5">

  
