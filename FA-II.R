
# ============================================================
# STEP 1: LOAD THE DATASET
# ============================================================

survey <- SDAUR_DATA

library(dplyr)
library(ggplot2)

View(survey)
dim(survey)
colnames(survey)
head(survey)
str(survey)
summary(survey)


# ============================================================
# STEP 2: DATA CLEANING
# ============================================================

# Remove extra spaces from column names
colnames(survey) <- trimws(colnames(survey))

# Check missing values
anyNA(survey)
sum(is.na(survey))
colSums(is.na(survey))

# Check blank responses
sapply(survey, function(x) sum(trimws(x) == "", na.rm = TRUE))

# Convert blank responses to NA
survey[survey == ""] <- NA

# Check missing values again
colSums(is.na(survey))


# ============================================================
# DATASET 1: SLEEP, PRODUCTIVITY AND CONCENTRATION
# Research Question 1
# ============================================================

sleep_data <- select(survey,
                     sleep_hours,
                     productivity,
                     concentration)

# Remove missing observations for this analysis
sleep_data <- na.omit(sleep_data)

# Summary statistics and correlations
Dataset1 <- data.frame(
  Output = c(
    "Average Sleep Hours",
    "Median Sleep Hours",
    "Standard Deviation of Sleep",
    "Average Productivity",
    "Average Concentration",
    "Correlation: Sleep and Productivity",
    "Correlation: Sleep and Concentration"
  ),
  Value = c(
    mean(sleep_data$sleep_hours),
    median(sleep_data$sleep_hours),
    sd(sleep_data$sleep_hours),
    mean(sleep_data$productivity),
    mean(sleep_data$concentration),
    cor(sleep_data$sleep_hours, sleep_data$productivity),
    cor(sleep_data$sleep_hours, sleep_data$concentration)
  )
)

# Display Dataset 1 output
print(Dataset1)
summary(sleep_data)

# Graph 1: Distribution of sleep hours
Graph1 <- ggplot(sleep_data, aes(x = sleep_hours)) +
  geom_histogram(bins = 8, fill = "skyblue",
                 color = "white") +
  labs(
    title = "Distribution of Daily Sleep Hours",
    x = "Sleep duration (hours)",
    y = "Number of students"
  ) +
  theme_minimal()

print(Graph1)

# Graph 2: Sleep and productivity
Graph2 <- ggplot(sleep_data,
                 aes(x = sleep_hours, y = productivity)) +
  geom_point(color = "darkblue", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", color = "orange") +
  labs(
    title = "Sleep Hours vs Productivity",
    x = "Sleep duration (hours)",
    y = "Productivity rating"
  ) +
  theme_minimal()

print(Graph2)

# Graph 3: Sleep and concentration
Graph3 <- ggplot(sleep_data,
                 aes(x = sleep_hours, y = concentration)) +
  geom_point(color = "purple", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", color = "darkorange") +
  labs(
    title = "Sleep Hours vs Concentration",
    x = "Sleep duration (hours)",
    y = "Concentration rating"
  ) +
  theme_minimal()

print(Graph3)


# ============================================================
# DATASET 2: COMMUTING, STUDY TIME AND PRODUCTIVITY
# Research Question 2
# ============================================================

travel_data <- select(survey,
                      commute_hours,
                      study_hours,
                      productivity)

# Remove missing observations for this analysis
travel_data <- na.omit(travel_data)

# Summary statistics and correlations
Dataset2 <- data.frame(
  Output = c(
    "Average Commuting Hours",
    "Median Commuting Hours",
    "Standard Deviation of Commuting",
    "Average Study Hours",
    "Average Productivity",
    "Correlation: Commute and Study",
    "Correlation: Commute and Productivity"
  ),
  Value = c(
    mean(travel_data$commute_hours),
    median(travel_data$commute_hours),
    sd(travel_data$commute_hours),
    mean(travel_data$study_hours),
    mean(travel_data$productivity),
    cor(travel_data$commute_hours, travel_data$study_hours),
    cor(travel_data$commute_hours, travel_data$productivity)
  )
)

# Display Dataset 2 output
print(Dataset2)
summary(travel_data)

# Graph 1: Distribution of commuting hours
Graph4 <- ggplot(travel_data, aes(x = commute_hours)) +
  geom_histogram(bins = 8, fill = "lightgreen",
                 color = "white") +
  labs(
    title = "Distribution of Daily Commuting Hours",
    x = "Commuting time (hours per day)",
    y = "Number of students"
  ) +
  theme_minimal()

print(Graph4)

# Graph 2: Commuting and study time
Graph5 <- ggplot(travel_data,
                 aes(x = commute_hours, y = study_hours)) +
  geom_point(color = "darkgreen", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", color = "orange") +
  labs(
    title = "Commuting Time vs Study Time",
    x = "Commuting time (hours per day)",
    y = "Study time (hours per day)"
  ) +
  theme_minimal()

print(Graph5)

# Graph 3: Commuting and productivity
Graph6 <- ggplot(travel_data,
                 aes(x = commute_hours, y = productivity)) +
  geom_point(color = "darkred", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", color = "navy") +
  labs(
    title = "Commuting Time vs Productivity",
    x = "Commuting time (hours per day)",
    y = "Productivity rating"
  ) +
  theme_minimal()

print(Graph6)


# ============================================================
# DATASET 3: SHORT-GAP ACTIVITIES AND PRODUCTIVITY
# Research Question 3
# ============================================================

activity_data <- select(survey,
                        gap_activity,
                        productivity)

# Remove missing observations for this analysis
activity_data <- na.omit(activity_data)

# Count students in each activity category
table(activity_data$gap_activity)

# Calculate average productivity and student count
Dataset3 <- activity_data %>%
  group_by(gap_activity) %>%
  summarise(
    Average_Productivity = mean(productivity),
    Students = n()
  )

# Display Dataset 3 output
print(Dataset3)

# Graph 1: Most common short-gap activities
Graph7 <- ggplot(activity_data,
                 aes(x = gap_activity, fill = gap_activity)) +
  geom_bar() +
  coord_flip() +
  labs(
    title = "How Students Use Their Short Gaps",
    x = "Short-gap activity",
    y = "Number of students",
    fill = "Activity"
  ) +
  theme_minimal()

print(Graph7)

# Graph 2: Average productivity by activity
Graph8 <- ggplot(Dataset3,
                 aes(x = gap_activity,
                     y = Average_Productivity,
                     fill = gap_activity)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average Productivity by Short-Gap Activity",
    x = "Short-gap activity",
    y = "Average productivity rating",
    fill = "Activity"
  ) +
  theme_minimal()

print(Graph8)


# ============================================================
# FINAL OUTPUT CHECK
# ============================================================

# View the three result datasets
View(Dataset1)
View(Dataset2)
View(Dataset3)

# View the cleaned analysis data
View(sleep_data)
View(travel_data)
View(activity_data)
