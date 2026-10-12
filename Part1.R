library(ggplot2)
library(dplyr)

# Load dataset
data1 = read.csv("SDAUR_DATA.csv")

# Select required columns
data1$Study_Time = trimws(as.character(data1[,9]))
data1$Productivity = data1[,20]

# Convert study time into numerical values
data1$Study_Hours = NA

# Single values
data1StudyHours[which(data1Study_Time == "0")] = 0
data1StudyHours[which(data1Study_Time == "1")] = 1
data1StudyHours[which(data1Study_Time == "1.5")] = 1.5
data1StudyHours[which(data1Study_Time == "2")] = 2
data1StudyHours[which(data1Study_Time == "2hrs")] = 2
data1StudyHours[which(data1Study_Time == "2 Hours")] = 2
data1StudyHours[which(data1Study_Time == "3")] = 3
data1StudyHours[which(data1Study_Time == "3 hours")] = 3
data1StudyHours[which(data1Study_Time == "3 hrs")] = 3
data1StudyHours[which(data1Study_Time == "4")] = 4
data1StudyHours[which(data1Study_Time == "4 hr")] = 4

# Range values: use the midpoint
data1StudyHours[which(data1Study_Time == "01-Feb")] = 1.5
data1StudyHours[which(data1Study_Time == "1 or 2")] = 1.5
data1StudyHours[which(data1Study_Time == "1 to 3")] = 2

data1StudyHours[which(data1Study_Time == "02-Mar")] = 2.5
data1StudyHours[which(data1Study_Time == "2-3 hours")] = 2.5
data1StudyHours[which(data1Study_Time == "2-3 hrs")] = 2.5
data1StudyHours[which(data1Study_Time == "2-3 Hours")] = 2.5
data1StudyHours[which(data1Study_Time == "2 to 3 hours")] = 2.5

data1StudyHours[which(data1Study_Time == "3-4 hrs")] = 3.5
data1StudyHours[which(data1Study_Time == "4-5hrs")] = 4.5

# Keep complete observations
data2 = data1 %>%
  filter(!is.na(Study_Hours), !is.na(Productivity))

# Check cleaned data
View(data2)
summary(data2$Study_Hours)
summary(data2$Productivity)

# Find correlation
cor(data2StudyHours,data2Productivity)

# GRAPH 1: Scatter plot with trend line
ggplot(data2, aes(x = Study_Hours, y = Productivity)) +
  geom_point(color = "darkcyan", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", color = "navy", fill = "lightblue") +
  labs(
    title = "Study Time and Student Productivity",
    subtitle = "Relationship between academic study time and productivity",
    x = "Study Time Outside Classes (Hours)",
    y = "Productivity Rating (1-10)"
  ) +
  theme_minimal()

# GRAPH 2: Distribution of study hours
ggplot(data2, aes(x = Study_Hours)) +
  geom_histogram(
    binwidth = 1,
    boundary = 0,
    fill = "darkcyan",
    color = "white"
  ) +
  labs(
    title = "Distribution of Daily Study Time",
    x = "Study Time Outside Classes (Hours)",
    y = "Number of Students"
  ) +
  theme_minimal()

# GRAPH 3: Distribution of productivity ratings
ggplot(data2, aes(y = Productivity)) +
  geom_boxplot(
    fill = "lightblue",
    color = "navy",
    width = 0.4
  ) +
  labs(
    title = "Distribution of Student Productivity",
    x = "",
    y = "Productivity Rating (1-10)"
  ) +
  theme_minimal()

# GRAPH 4: Average productivity for each study duration
study_summary = data2 %>%
  group_by(Study_Hours) %>%
  summarise(
    Average_Productivity = mean(Productivity),
    Number_of_Students = n()
  )

print(study_summary)

ggplot(study_summary,
       aes(x = factor(Study_Hours), y = Average_Productivity)) +
  geom_col(fill = "darkcyan", width = 0.7) +
  labs(
    title = "Average Productivity by Study Duration",
    x = "Study Time Outside Classes (Hours)",
    y = "Average Productivity Rating"
  ) +
  theme_minimal()



