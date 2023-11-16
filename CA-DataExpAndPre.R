# CA - 
# Lecturer's Name: 
# Student Name:
#

# Read csv
data <- read.csv("C:/Users/henri/Documents/CA-DataExpAndPre/CovidVacRate.csv")

# This dataset contains information about vaccination rates over Ireland

# Display data
head(data)

# Check structure
str(data)

# Check missing values
sapply(data, function(x) sum(is.na(x)))

# Min-Max Normalization
data$VALUE_MinMax <- (data$VALUE - min(data$VALUE)) / (max(data$VALUE) - min(data$VALUE))

