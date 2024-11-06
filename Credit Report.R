install.packages(c("dplyr", "tidyr", "data.table", "stringr", "readr", "readxl", "DBI", "RSQLite", 
                   "ggplot2", "plotly", "cowplot", "patchwork", "lmtest", "sandwich", "broom", 
                   "caret", "randomForest", "xgboost", "e1071", "forecast", "zoo", "xts", "timetk",
                   "rmarkdown", "knitr", "shiny", "lubridate", "janitor", "here", "outliers"))

install.packages("Amelia")

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


##Univariate Analysis

# Histogram for numerical variables
for (col in names(Credit)[numeric_columns]) {
  hist(Credit[[col]], main = paste("Histogram of", col), xlab = col, col = "lightblue")
}

# Bar plot for categorical variables
categorical_columns <- sapply(Credit, is.factor)
for (col in names(Credit)[categorical_columns]) {
  barplot(table(Credit[[col]]), main = paste("Bar Plot of", col), col = "lightgreen")
}

##Bivariate analysis
# Scatter plot for pairs of numerical variables
# Plot age against checking balance
pairs(Credit[, c("age", "checking_balance")], main = "Scatter Plot of Age vs Checking Balance")

# Plot age against amount
pairs(Credit[, c("age", "amount")], main = "Scatter Plot of Age vs amount")


## Correlation Analysis
# Correlation matrix

# Encode `credit_history` as numeric
Credit$credit_history_numeric <- as.numeric(factor(Credit$credit_history, levels = c("critical", "fully repaid", "delayed", "repaid", "fully repaid this bank")))

# Calculate correlation between age and credit_history_numeric
correlation_age_credit_history <- cor(Credit$age, Credit$credit_history_numeric, use = "complete.obs")
print(correlation_age_credit_history)

cor_matrix <- cor(Credit[, numeric_columns], use = "complete.obs")
print(cor_matrix)

# Visualize with a heatmap
library(corrplot)
corrplot(cor_matrix, method = "color", addCoef.col = "black")

#Clean New Window
plot.new(); dev.off()
corrplot(cor_matrix, method = "color", addCoef.col = "black", main = "Correlation: Age and Credit History")
par(mar = c(0.05, 0.05, 0.05, 0.05))
par(mfrow = c(1, 1))
par(pin = c(30, 30))
corrplot(cor_matrix, method = "color", addCoef.col = "black", main = "Correlation: Age and Credit History", tl.cex = 0.5)