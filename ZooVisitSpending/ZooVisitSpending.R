# This workflow visualizes zooSpending data it with histograms, and 
# correlation analysis, then builds a multiple linear regression model 
# predicting membership. The coefficient in the summaries give us an idea of 
# the impact of each predictor variable. It visualizes relationships with 
# corrplot and tests regression assumptions of multicollinearity using olsrr. 

# Install the tidyverse, corrplot, and olsrr packages 
# (comment out the install lines after writing them)
# install.packages("tidyverse")
# install.packages("dummies", repos = NULL, type = "source")
# install.packages("corrplot")
# install.packages("olsrr")

# Load the tidyverse, corrplot, and olsrr libraries
library(tidyverse)
library(corrplot)
library(olsrr)
library(ggplot2)

# Set the working directory to your Lab03 folder
setwd("/Users/lovely/Documents/MIS 545 Data Mining/Lab03")

# Read ZooVisitSpending.csv into a tibble called zooSpending 
# (use the data types suggested at the bottom of these assignment instructions).
# Ensure you use the correct read_csv() function and not readcsv() or read.csv().
zooSpending <- read_csv("ZooVisitSpending.csv", col_types = 'niil', 
                        col_names = TRUE)

# Display zooSpending in the console
print(zooSpending)

# Display the structure of zooSpending in the console
str(zooSpending)

# Display the summary of zooSpending in the console
summary(zooSpending)

# Recreate the displayAllHistograms() function as shown in the video 
# demonstration.
displayAllHistograms <- function(tibbleDataset) {
  tibbleDataset %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot(aes(x = value, fill = key)) + geom_histogram(color = "black") +
    facet_wrap( ~ key, scales = "free") +
    theme_minimal()
}

# Call the displayAllHistograms() function, passing in zooSpending as an 
# argument
displayAllHistograms(zooSpending)

# Display a correlation matrix of zooSpending rounded to two decimal places
zooSpendingCorrMatrix <- round(cor(zooSpending), 2)
zooSpendingCorrMatrix

# Display a correlation plot using the "number" method and limit output to the 
# bottom left
corrplot(zooSpendingCorrMatrix, method = "number", type = "lower")

# Generate the linear regression model and save it in an object called 
# zooSpendingModel
# zooSpendingModel <- lm(formula = Member ~ ., data = zooSpending)
zooSpendingModel <- lm(formula = VisitSpending ~ PartySize + MilesFromZoo 
                       + Member, data = zooSpending)

# Display the beta coefficients for the model on the console
zooSpendingModel$coefficients

# Display the linear regression model results using the summary() function
summary(zooSpendingModel)

# Test for multicollinearity using the ols_vif_tol() function
ols_vif_tol(zooSpendingModel)
