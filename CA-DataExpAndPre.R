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



install.packages("devtools")
devtools::install_github("ropensci/skimr")

library(ggplot2)
library(skimr)
skimr::skim(data)

data$Age <- as.numeric(data$Age.Group)
str(data)



# Replace '5 - 11 years' with random ages between 5 and 11
data$Age.Group <- ifelse(data$`Age.Group` == '5 - 11 years',
                 sample(5:11, sum(data$`Age.Group` == '5 - 11 years'), replace = TRUE),
                 data$Age.Group)

# Viewing the updated 'Age' column
head(data$Age.Group)


