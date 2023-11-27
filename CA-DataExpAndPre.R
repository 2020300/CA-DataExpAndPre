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
data$Vaccination_Code <- ifelse(data$Statistic.Label == "Fully Vaccinated", 1, 2)

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


# Calculate mix-max normalization

# Numerical variables
numerical_variables <- c("Age.Group", "VALUE")

# Min-Max Normalization
minmax_values <- lapply(data[, numerical_variables], function(col) (col - min(col, na.rm = TRUE)) / (max(col, na.rm = TRUE) - min(col, na.rm = TRUE)))

# Z-score Standardization
zscore_values <- lapply(data[, numerical_variables], scale)

# Robust Scaling
robust_values <- lapply(data[, numerical_variables], function(col) (col - median(col, na.rm = TRUE)) / IQR(col, na.rm = TRUE))

# Display the transformed values
cat("Min-Max Normalization:\n")
print(minmax_values)

cat("\nZ-score Standardization:\n")
print(zscore_values)

cat("\nRobust Scaling:\n")
print(robust_values)



# Create a line plot
ggplot(data, aes(x = seq_along(minmax_values), y = (minmax_values))) +
  geom_line() +
  labs(title = "Min-Max Normalization of C02076V03371", x = "Observation", y = "Normalized Value")

