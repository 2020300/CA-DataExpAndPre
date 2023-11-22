# CA - 
# Lecturer's Name: 
# Student Name:
#


install.packages("devtools")
devtools::install_github("ropensci/skimr")

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

# Transform age into numeric value
data$Age.Group <- as.numeric(data$Age.Group)
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

# Display the modified dataset
print(data)

# Now we age, month and year fixed we can start manipulating our data

hist(data$Age.Group, xlab = "Age", main = "Age Distribution")
