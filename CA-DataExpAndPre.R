# CA - 
# Lecturer's Name: 
# Student Name:
#

#Import libraries needed
library(lubridate)
library(ggplot2)
library(skimr)
library(tidyr)
library(scales)
library(GGally)
library(dplyr)

# Read csv
data <- read.csv("C:/Users/henri/Documents/CA-DataExpAndPre/CovidVacRate.csv")

# This dataset contains information about vaccination rates over Ireland

# Using skmir to understand the data: 
skimr::skim(data)

# Since we have way too many columns, I will store the first 7500 columns and store in a different dataset
data <- head(data, 7500)
# Display data

head(data)

# Check structure
str(data)

## DATA PREPARING ##

# Fixing VALUE column by replacing NA values with 0
column_name <- "VALUE" 
data[[column_name]][is.na(data[[column_name]])] <- 0


# Replace '5 - 11 years' with random ages between 5 and 11
data$Age.Group <- ifelse(data$`Age.Group` == '5 - 11 years',
                 sample(5:11, sum(data$`Age.Group` == '5 - 11 years'), replace = TRUE),
                 data$Age.Group)

# Replace '12 years and over' with random ages between 12 and 60
data$Age.Group <- ifelse(data$`Age.Group` == '12 years and over',
                         sample(12:60, sum(data$`Age.Group` == '12 years and over'), replace = TRUE),
                         data$Age.Group)

# Viewing the updated 'Age' column
head(data$Age.Group)

# Separate the Month column into Month and Year
data <- separate(data, Month, into = c("Year", "Month"), sep = " ")

# Assign "Fully Vaccinated" as 1 and not Fully Vaccinated 2, to make it easier to manipulate the data
data$Vaccination_Code <- ifelse(data$Statistic.Label == "Fully Vaccinated", 0, 1)
tail(data$Vaccination_Code)

# Transform age and other columns with wrong formation into numeric value
data$Age.Group <- as.numeric(data$Age.Group)
data$VALUE <- as.numeric(data$VALUE)
data$Year <- as.numeric(data$Year)


# Removing updated data
data <- subset(data, select = -C03898V04649)

# Display the modified dataset
str(data)
head(data)
summary(data)

# Now we age, month and year fixed we can start manipulating our data
hist(data$Vaccination_Code, xlab = "Vaccination_Code", main = "Age Distribution")

#Task B - Calculate mean, median, minimum, maximum and sd 

# Calculate mean, median, minimum, maximum, and standard deviation for Age
mean_Age <- mean(data$Age.Group)
median_Age <- median(data$Age.Group)
min_Age <- min(data$Age.Group)
max_Age <- max(data$Age.Group)
sd_Age <- sd(data$Age.Group)

# Print the results
cat("\nMean of Age:", mean_Age, "\n")
cat("Median of Age:", median_Age, "\n")
cat("Minimum of Age:", min_Age, "\n")
cat("Maximum of Age:", max_Age, "\n")
cat("Standard Deviation of Age:", sd_Age, "\n")

# Calculate mean, median, minimum, maximum, and standard deviation for Value
mean_Value <- mean(data$VALUE)
median_Value <- median(data$VALUE)
min_Value <- min(data$VALUE)
max_Value <- max(data$VALUE)
sd_Value <- sd(data$VALUE)

# Print the results
cat("\nMean of VALUE:", mean_Value, "\n")
cat("Median of VALUE:", median_Value, "\n")
cat("Minimum of VALUE:", min_Value, "\n")
cat("Maximum of VALUE:", max_Value, "\n")
cat("Standard Deviation of VALUE:", sd_Value, "\n")

# End of Task B

# Task C - MinMax Normalization, Z-Score Standardization and Robust Scaling
# Numerical variables
numerical_variables <- c("Age.Group")

# Min-Max Normalization
minmax_values <- (data[, column_name] - min(data[, column_name], na.rm = TRUE)) /
  (max(data[, column_name], na.rm = TRUE) - min(data[, column_name], na.rm = TRUE))

# Z-score Standardization
zscore_values <- scale(data[, column_name])

# Robust Scaling
robust_values <- (data[, column_name] - median(data[, column_name], na.rm = TRUE)) / IQR(data[, column_name], na.rm = TRUE)

# Display the transformed values
cat("Min-Max Normalization:\n")
#print(minmax_values)
hist(data$Age.Group, type = "l", main = "Original Values", xlab = "Index", ylab = "Original Line Plot")
hist(minmax_values, type = "l", main = " After Min-Max Normalization", xlab = "Index", ylab = "Min-Max Normalized Scale")


cat("\nZ-score Standardization:\n")
#print(zscore_values)
hist(data$Age.Group, main = "Original values", ylab = "")
# Boxplot for z-score normalized values
hist(zscore_values, main = "Z-Score Normalized Values", ylab = "Z-Score")


cat("\nRobust Scaling:\n")
print(robust_values)
hist(data$Age.Group, main = "Original values", ylab = "")
hist(robust_values, main = "Robust Scaling Values", ylab = "Robust Scaling")

# End of Task C 

# Task D - Heatmaps and correlation
data_2 <- cbind(data$Age.Group, data$Vaccination_Code)    
ggpairs(data_2)

corr_coefficients = data_2
corr_coefficients

heatmap(corr_coefficients)

# Calculate the correlation matrix
cor_matrix <- cor(data_2, use = "complete.obs")

# Create a correlation heatmap
corrplot(cor_matrix, method = "color", title = "Correlation Heatmap")

# End of the task D

# Task E - Create graphs to explore data
# Plot graph created to see which Month people were most vaccinated
ggplot(data = data) + 
  geom_bar(aes(x = Month))

# Explore which month people under 20 and over 35 have been vaccinated
data_u20 <- data[data$Age.Group <= 20, ]
data_o35 <- data[data$Age.Group >= 35, ]

# Plot for people under 20 years old
ggplot(data = data_u20) + 
  # geom_bar(aes(x = Month))

geom_density(aes(x = Month)) +
  labs(title = "Density Plot of Numerical Variable for People Under 20", x = "Numerical Variable", y = "Density")

# End of the Task E

# Task F - PCA
# Scale the date
