#load libraries 
library(tidyverse) #calculations
library(lubridate) #dates 
library(hms) #time
library(data.table) #exporting data frame
library(dplyr)

#Import data to R
Credit <- read.csv("C:\\Users\\HP\\Downloads\\Portfolio_analyst\\GermanCredit.csv")

# Convert to a unified format in years as a numeric value
Credit$employment_length_years <- sapply(Credit$employment_length, function(x) {
  if (grepl("year", x)) {
    # Extract the number of years directly
    years <- as.numeric(gsub(" year.*", "", x))
    return(years)
  } else if (grepl("month", x)) {
    # Extract the number of months and convert to years
    months <- as.numeric(gsub(" month.*", "", x))
    return(months / 12)
  } else {
    # In case of unexpected format, return NA
    return(NA)
  }
})

#View first rows
head(Credit)

#Overview of Data Structure
str(Credit)

#Summary Statistics for the data
summary(Credit)

#Convert missing values to NA
Credit[Credit == ""] <- NA

# Check for missing values in each column
colSums(is.na(data))
 
# Visualize missing data
library(Amelia)
missmap(Credit, main = "Missing Values Map", col = c("yellow", "black"))

# Boxplot for each numerical variable
numeric_columns <- sapply(Credit, is.numeric)
boxplot(Credit[, numeric_columns], main = "Boxplot of Numerical Variables", las = 2)

# Plot distribution of the 'credit_history' variable to age_group
ggplot(Credit, aes(x = factor(age_group), fill = factor(credit_history))) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Credit History by Age", 
       x = "Age", 
       y = "Count") +
  theme_minimal()

library(ggplot2)

# Plot distribution of "Default" Variable by number of Dependents
ggplot(Credit, aes(x = factor(dependents), fill = factor(default))) +
  geom_bar(position = "dodge") +
  labs(title = "Effect of Dependents on Defaults",
       x = "Dependents",
       y = "Count") +
  theme_minimal()

# Define age groups and their labels
age_breaks <- c(0, 18, 30, 45, 60, Inf)
age_groups <- c("0-17", "18-29", "30-44", "45-59", "60+")

# Create a new column 'age_group' using the 'cut' function
Credit$age_group <- cut(Credit$age, breaks = age_breaks, labels = age_groups, right = FALSE)

# Plot distribution of "Default" Variable by age groups
ggplot(Credit, aes(x = factor(age_group), fill = factor(default))) +
  geom_bar(position = "dodge") +
  labs(title = "Effect of Age on Defaults",
       x = "Age",
       y = "Count") +
  theme_minimal()


# Plot distribution of "Default" Variable by credit history
ggplot(Credit, aes(x = factor(credit_history), fill = factor(default))) +
  geom_bar(position = "dodge") +
  labs(title = "Effect of Credit History on Defaults",
       x = "Credit_History",
       y = "Count") +
  theme_minimal()