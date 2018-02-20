# Name of Quantlet: SPL_Descriptive 
# Published in: Statistical Programming Languages 
# Description: 'Includes functions to get an overview of summary statistics for the variables and an analysis of correlations of variables' 
# Keywords: summary, descriptive, function, correlation, ggplot2, visualisation Author: Mona Timmermann
# Submitted: Fr, Aug 18 2017


# This script helps to get a first overview of the data set and correlations between variables




# read in the datasets
source("Helperscript_LoadDatasets.R")

# dataset1 = messy data dataset2 = clean data dataset3 = clean data and standardised numeric variables



# The following code creates data frames containing summary statistics for numeric/ categorical variables and
# therefore gives us the opportunity to have an overview of all variables either before and after data cleaning



## --------------------------FOR NUMERIC VARIABLES-------------------------- ##
if (!require("Hmisc")) install.packages("Hmisc")
library("Hmisc")
if (!require("plyr")) install.packages("plyr")
library("plyr")

# Define desired operations within a function:
num_summaries = function(x) {
    missing_perc = (sum(is.na(x))/length(x)) * 100
    min = min(x, na.rm = TRUE)
    mean = mean(x, na.rm = TRUE)
    median = median(x, na.rm = TRUE)
    max = max(x, na.rm = TRUE)
    sd = sd(x, na.rm = TRUE)
    out = c(missing_perc, min, mean, median, max, sd)
    return(out)
}

# Since we want to apply the function to the whole dataset, we use numcolwise(), which turns a function that
# operates on a vector into a function that operates column-wise on numeric variables in a a data.frame:
options(digits = 0)  # to make the data frame more readable
numoverview = numcolwise(num_summaries)(dataset1)

# We want to save the results within a dataframe with the variables as rows, not columns:
numoverview = as.data.frame(t(numoverview))
colnames(numoverview) = c("MissingPercentage", "Minimum", "Mean", "Median", "Maximum", "SD")

# We now order the dataframe by the the percentage of missing values:
numoverview = numoverview[order(numoverview$MissingPercentage, decreasing = TRUE), ]



## --------------------------FOR CATEGORICAL VARIABLES--------------------------## We do exactly the same as above,
## just that we now define a function that calculates the percentage of missing values, the unique values and
## number of unique values for categorical variables in the dataset and apply catcolwise:

cat_summaries = function(x) {
    missing_perc = (sum(is.na(x))/length(x)) * 100
    unique = unique(x)
    unique_n = length(unique)
    uniques = paste(unique, collapse = ",")
    out = c(missing_perc, unique_n, uniques)
    return(out)
}

catoverview = catcolwise(cat_summaries)(dataset1)
catoverview = as.data.frame(t(catoverview))
colnames(catoverview) = c("MissingPercentage", "UniqueValues_Amount", "UniqueValues")

catoverview = catoverview[order(catoverview$MissingPercentage, decreasing = TRUE), ]

## --------------------------CORRELATION ANALYSIS--------------------------## The following produces a matrix
## containing the correlation of all numeric variables.  It can be seen that many variables are highly correlated,
## especially locational variables like big_church_km and additional_education_kilometer (seems obvious and should
## be expected):
idx.numeric = sapply(dataset2, is.numeric)
fullcorrs = cor(dataset2[, idx.numeric])

# There are many ways to visualise correlations, we chose to use the package lattice.  We do, however, have to
# reduce the number of variables for the plot to be readable, e.g.:
if (!require("lattice")) install.packages("lattice")
library("lattice")

# Example of a plot:
quartz()
corrset2 = dataset2[, colnames(dataset2) %in% c("area_meterprice", "metro_min_walk", "big_market_km")]
corrs2 = cor(corrset2)
lattice::levelplot(corrs2, at = (-10:10)/10)  # range from -1 to +1
