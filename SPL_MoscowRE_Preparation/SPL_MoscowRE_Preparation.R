# Name of Quantlet: SPL_Preparation 
# Published in: Statistical Programming Languages 
# Description: Demonstrates the cleaning of the housing dataset provided by Sberbank 
# Keywords: preprocessing, transformation, missing, housing, real estate 
# Author: Alisa Kolesnikova 
# Submitted: Fr, Aug 18 2017


# Cleaning up the space
rm(list = ls())
options(max.print = 1000000)

## --------------------------Packages--------------------------- ##
if (!require('dplyr')) install.packages('dplyr')
library('dplyr')
if (!require('ade4')) install.packages('ade4')
library('ade4')
if (!require('data.table')) install.packages('data.table')
library('data.table')


## --------------------------Data Import-------------------------- ##

# Reading in the transactional data. The dataset is provided by the Sberbank of Russia and consists of around
# 30 000 observation and 292 variables.
rus = readRDS('train')
str(rus)



## --------------------------Transformations, Missing Value Imputations-------------------------- ##

# Convert time variable to date format:
rus$timestamp = as.Date(rus$timestamp)

# Checking for missing values
missing = data.frame(sapply(rus, function(x) sum(is.na(x)) * 100/length(x)))
missing$feature = names(rus)
missing$num = c(1:nrow(missing))
colnames(missing) = c('missing_ratio', 'feature', 'num')
missing = missing[, c('num', 'feature', 'missing_ratio')]
print(missing[missing$missing_ratio != 0, ], row.names = F)

# 50 variables have missing values, some up to 47%. Most important ones supposed to be life_sq(20%), max_floor
# (31%), material(31%), build_year (44%), num_room (31%), state (44%) These values have to be imputed, however,
# as the key orienteer will be the full_sq, it has to examined for inconsistencies first

# Check the size of the apartment: full_sq:
table(rus$full_sq)
# Any values below 12 sqm or above 400 sqm should be considered false. As this is a key variables, they will be
# deleted, not imputed. For the facilitation of computation all the squres above 400 are considered outliers:
rus = rus[rus$full_sq > 12 & rus$full_sq < 400, ]


# Life_sq imputation: it is not only the NA, it's also the impossible values like '1' that have to be imputed.
# We shall consider false values less that 8 and bigger than 400 and impute them using the average ratio of
# life_sq to full_sq:
table(rus$life_sq)
meanratio = mean(!is.na(rus$life_sq)/rus$full_sq)
rus$life_sq[is.na(rus$life_sq) | rus$life_sq < 8 | rus$life_sq > 400] = (rus$full_sq[is.na(rus$life_sq) | rus$life_sq < 
    8 | rus$life_sq > 400]) * meanratio
rus$life_sq = round(rus$life_sq)


# Also inconsistencies between the full_sq and life_sq have to be taken into account. There are 25 observations
# with life_sq exceeding full_sq, manual imputation is possible but we will get rid of them to save time, as
# well as of 12 obs that show difference of over 100 sqm between these two values:
rus = rus[rus$full_sq >= rus$life_sq, ]
rus = rus[(rus$full_sq - rus$life_sq) < 100, ]


# There are some observations with most of the key variables missing simultaneously. To avoid mistakes with
# imputation, they will be deleted (around 166 obs). There are tall buildings in Moscow, but 77th floor is too
# high - will be deleted:
rus = rus[!(is.na(rus$floor) & is.na(rus$max_floor) & is.na(rus$build_year)), ]
table(rus$floor)
rus = rus[!(rus$floor == 77), ]


# Max_floor will be imputed using floor, deleting inconsistencies:
rus$max_floor[is.na(rus$max_floor) | rus$max_floor == 0] = rus$floor[is.na(rus$max_floor) | rus$max_floor == 0] + 
    2
rus$max_floor[rus$max_floor < rus$floor] = rus$floor[rus$max_floor < rus$floor] + 1
rus = rus[(rus$max_floor - rus$floor) < 40, ]


# Now to material and state. Missing variables will be saved as unknown levels:
table(rus$state)
rus$state = as.character(rus$state)
rus$state[rus$state == '33'] = '3'
rus$state[is.na(rus$state)] = 'un'
rus$state = as.factor(rus$state)

table(rus$material)
rus$material[rus$material == '3'] = '1'
rus$material = as.character(rus$material)
rus$material[is.na(rus$material)] = 'un'
rus$material = as.factor(rus$material)


# Imputing the build_year and transforming it to factor variable after classification with a little year_class
# function:

yearclass = function(data, min_year, max_year) {
    data$build_year[is.na(data$build_year)] = 3000
    data$build_year[data$build_year > max_year | data$build_year < min_year] = 3000
    data$buildage = ifelse(data$build_year <= 1910, 'veryold', ifelse(data$build_year <= 1950, 'old', ifelse(data$build_year <= 
        1990, 'medium', ifelse(data$build_year <= 2010, 'new', ifelse(data$build_year <= 2018, 'verynew', 'unknown')))))
    data$buildage = as.factor(data$buildage)
    data$build_year = NULL
    return(data)
}

rus = yearclass(rus, 1890, 2019)
table(rus$buildage)


# Num_kitchen will be imputed as a mean ratio from full_sq (approx 14%):
rus$kitch_sq[is.na(rus$kitch_sq) | rus$kitch_sq < 3 | rus$kitch_sq > 48] = rus$full_sq[is.na(rus$kitch_sq) | rus$kitch_sq < 
    3 | rus$kitch_sq > 48] * 0.14
rus$kitch_sq = round(rus$kitch_sq)


# Num_room will be imputed with a regression:
num_regr = lm(num_room ~ full_sq + life_sq + floor + kitch_sq, rus)
rus$num_room[is.na(rus$num_room)] = predict(num_regr, rus)
rus$num_room = round(rus$num_room)
rus$num_room[rus$num_room < 1] = 1
table(rus$num_room)
rus$num_room[rus$num_room > 7 & rus$full_sq < 100] = 3


# Some variables are missing by sub_areas and are more than 40% - makes not sence to impute it:
rus$hospital_beds_raion = NULL
rus$cafe_sum_500_min_price_avg = NULL
rus$cafe_sum_500_max_price_avg = NULL
rus$cafe_avg_price_500 = NULL


# Imputing the rest with the mean:
rus$preschool_quota[is.na(rus$preschool_quota)] = mean(rus$preschool_quota, na.rm = TRUE)
rus$school_quota[is.na(rus$school_quota)] = mean(rus$school_quota, na.rm = TRUE)
mean_imp = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
rus[] = lapply(rus, mean_imp)
rus$sq_dif = NULL


# Save the result
saveRDS(rus, 'set1')

