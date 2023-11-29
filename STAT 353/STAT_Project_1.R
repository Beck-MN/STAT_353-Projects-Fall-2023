
# ---
# title: "STAT 353 Project 1"
# author: "Charles Beck Christensen"
# date: "2023-10-05"
# ---
  
#Setup (change file path as needed)
library(readxl)
CollegeScores4yr <- read_excel("C:/Users/cbchr/Downloads/CollegeScores4yr.xlsx")
View(CollegeScores4yr)

# Load required packages
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

# Filter data based on school type (Control column: Public vs. Private)
public_schools <- subset(CollegeScores4yr, Control == "Public")
private_schools <- subset(CollegeScores4yr, Control == "Private")

#Find the average cost and SD of In-State Tuition
result <- CollegeScores4yr %>%
  group_by(Control) %>%
  summarise(
    avg_in_state_tuition = mean(TuitionIn, na.rm = TRUE),
    sd_in_state_tuition = sd(TuitionIn, na.rm = TRUE)
  )

print(result)

#Find the average cost and SD of faculty salary
result <- CollegeScores4yr %>%
  group_by(Control) %>%
  summarise(
    avg_fac_salary = mean(FacSalary, na.rm = TRUE),
    sd_fac_salary = sd(FacSalary, na.rm = TRUE)
  )

print(result)


# Boxplot to visualize tuition fees comparison
ggplot() +
  geom_boxplot(data = public_schools, aes(x = "Public", y = TuitionIn, fill = "Public"), width = 0.3) +
  geom_boxplot(data = private_schools, aes(x = "Private", y = TuitionIn, fill = "Private"), width = 0.3) +
  labs(title = "Comparison of Tuition Fees (In-state) between Public and Private Schools",
       x = "School Type", y = "Tuition Fees (In-state)") +
  scale_fill_manual(values = c("Public" = "blue", "Private" = "red"))

# Scatterplot to visualize enrollment compared to tuition by control type
ggplot(CollegeScores4yr, aes(x = TuitionIn, y = Enrollment, color = Control)) +
  geom_point() +
  labs(title = "Enrollment vs. In-State Tuition by Control Type",
       x = "In-State Tuition",
       y = "Enrollment") +
  theme_minimal() +
  scale_color_manual(values = c("Public" = "blue", "Private" = "red"))

#Boxplot to visualize faculty salaries
ggplot() +
  geom_boxplot(data = public_schools, aes(x = "Public", y = FacSalary, fill = "Public"), width = 0.3) +
  geom_boxplot(data = private_schools, aes(x = "Private", y = FacSalary, fill = "Private"), width = 0.3) +
  labs(title = "Comparison of Faculty Salary between Public and Private Schools",
       x = "School Type", y = "Faculty Salary)") +
  scale_fill_manual(values = c("Public" = "blue", "Private" = "red"))

# Scatterplot to visualize enrollment compared to FacSalary by control type
ggplot(CollegeScores4yr, aes(x = FacSalary, y = Enrollment, color = Control)) +
  geom_point() +
  labs(title = "Enrollment vs. In-State Tuition by Control Type",
       x = "Faculty Salary",
       y = "Enrollment") +
  theme_minimal() +
  scale_color_manual(values = c("Public" = "blue", "Private" = "red"))