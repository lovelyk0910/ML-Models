# This workflow prepares and explores the dataset by handling missing values, 
# identifying outliers, transforming features (including dummy coding), 
# and generating visual insights. It follows standard data cleaning 
# and exploratory analysis practices in R, ensuring the data is structured 
# and ready for further analysis or modeling.

# Load the tidyverse and dummies libraries
# install.packages("tidyverse")
# install.packages("dummies", repos = NULL, type = "source")
library("tidyverse")
library("dummies")
library("ggplot2")

# Set the working directory to your Lab02 folder
setwd("/Users/lovely/Documents/MIS 545 Data Mining/Lab02")

# Read TireTread.csv into a tibble called tireTread1 (use the data types 
# suggested at the bottom of these assignment instructions). Ensure you use 
# the correct read_csv() function and not readcsv() or read.csv().
tireTread1 <- read_csv("TireTread.csv", 
                       col_types = 'cfnni') 

# Display tireTread1 in the console
print(tireTread1)

# Display the structure of tireTread1 in the console
str(tireTread1)

# Display the summary of tireTread1 in the console
summary(tireTread1)  # 6 NAs observed in UsageMonths

# Impute missing data for UsageMonths with the mean (see example on Nwanganga 
# textbook pg. 78) and store the result into a new tibble called tireTread2
tireTread2 <- tireTread1 %>% 
  mutate(UsageMonths=ifelse(is.na(UsageMonths), 
                                   mean(tireTread1$UsageMonths, na.rm=TRUE),
                                   UsageMonths))

# Run a summary on tireTread2 and view it in the data viewer to ensure that the
# missing values have been replaced with the mean
summary(tireTread2)  #0 NAs present in UsageMonths


# Determine outliers in the TreadDepth feature. Start by calculating outlier 
# min and max and store into variables called outlierMin and outlierMax.
quartile1 <- quantile(tireTread2$TreadDepth, 0.25, na.rm = TRUE)
print(quartile1)
quartile3 <- quantile(tireTread2$TreadDepth, 0.75, na.rm = TRUE)
print(quartile3)
iqrValue <- quartile3 - quartile1
print(iqrValue)
outlierMin <- quartile1 - 1.5 * iqrValue
print(outlierMin)
outlierMax <- quartile3 + 1.5 * iqrValue
print(outlierMax)

# Keep the outliers in the dataset, but add the outliers to their own tibble 
# called treadDepthOutliers
treadDepthOutliers <- tireTread2 %>%
  filter(TreadDepth < outlierMin | TreadDepth > outlierMax)
treadDepthOutliers  # 18 of 456 records are Outliers

# Normalize the UsageMonths feature by taking the log of UsageMonths into a new 
# feature called LogUsageMonths and store the additional column in a tibble 
# called tireTread3
tireTread3 <- tireTread2 %>% 
  mutate(LogUsageMonths = log(UsageMonths))
tireTread3

# Discretize TreadDepth into NeedsReplacing (tires with tread depth of less 
# than or equal to 1.6mm need replacing) and store into new tireTread4 tibble
tireTread4 <- tireTread3 %>% 
  mutate(NeedsReplacing = ifelse(TreadDepth <= 1.6, TRUE, FALSE))
tireTread4

# Dummy code the Position (LF, RF, LR, RR) feature. Start by converting 
# tireTread4 into a data frame
tireTreadDataFrame <- data.frame(tireTread4)

# Dummy code the Position using dummy.data.frame(), convert it back into a 
# tibble, and store the result into a new tireTread5 tibble
tireTread5 <- as_tibble(dummy.data.frame(tireTreadDataFrame, 
                                         names = "Position"))
tireTread5


# Use ggplot() to build a scatter plot of Miles (x) with TreadDepth (y). 
# Use the default point size, but change the point color to dark gray. 
# Add a linear best fit line to the plot and color it red. Add a title to
# the scatter plot, "Tire Miles and Tread Depth Scatter Plot"
ggplot(tireTread5, mapping= aes(x = Miles, y = TreadDepth)) +
  geom_point(alpha = 1.0, color = 'darkgrey') +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Miles", y = "Tread Depth", 
       title = "Tire Miles and Tread Depth Scatter Plot")


# The scatterplot shows a negative correlation between Tire Miles and Tread Depth, 
# indicating that tread depth decreases as mileage increases. This inverse relationship 
# reflects normal tire wear, where higher usage and friction gradually reduce tread depth.
