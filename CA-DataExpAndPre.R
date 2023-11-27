# CA - 
# Lecturer's Name: 
# Student Name:
#

#Import libraries needed
library(lubridate)
library(ggplot2)
library(skimr)
library(tidyr)
skimr::skim(data)

# Read csv
data <- read.csv("C:/Users/henri/Documents/CA-DataExpAndPre/CovidVacRate.csv")

# This dataset contains information about vaccination rates over Ireland

# Display data
head(data)

# Check structure
str(data)

## DATA PREPARING ##

# Transform age and other columns with wrong formation into numeric value
data$Age.Group <- as.numeric(data$Age.Group)
data$VALUE <- as.numeric(data$VALUE)
data$Year <- as.numeric(data$Year)

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

# Removing updated data
data <- subset(data, select = -C03898V04649)

# Display the modified dataset
str(data)
print(data)

# Now we age, month and year fixed we can start manipulating our data
hist(data$Age.Group, xlab = "Age", main = "Age Distribution")
