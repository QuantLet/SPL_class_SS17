# Name of Quantlet: SPL_Models 
# Published in: Statistical Programming Languages 
# Description: 'Applying eXtreme Gradient Boosting to three different sets of variables, 
# comparing performances on the validation set, and
# using Partial Least Squares Regression as a benchmark' 
# Keywords: prediction, boosting, machine, regression, trees, partial least squares, RMSE 
# Author: Mona Timmermann Submitted: Fr, Aug 18 2017


# We chose to apply eXreme Gradient Boosting because it is supposed to be more robust than other techiques and
# has often not only shown high predictive accuracy but also computational efficiency

# set random number generator
set.seed(194)
# Install and load packages for XGB
if (!require("caret")) install.packages("caret")
library("caret")
if (!require("xgboost")) install.packages("xgboost")
library("xgboost")
if (!require("plyr")) install.packages("plyr")
library("plyr")



# we apply the following script on different data sets load all datasets and then define what dataset should be
# used in the following
source("Helperscript_LoadDatasets.R")
# dataset1 = messy data, dataset2 = clean data, dataset3 = clean data and standardised numeric variables
# dataset4: use dataset3 and set target variable as sqrt(price_doc) in caret's train function & then always
# adapt the predictions by using ^2, e.g. xgb.pred = predict(xgb, newdata = val)^2

set1 = dataset2  # change depending on which dataset you want to use


# fyi: we have trained eXtreme gradient boosting regression trees on three sets of variables: 1) all variables,
# including many highly correlated variables 2) variables selected using xgb variable importance 3) few
# hand-selected variables, loosely based on theoretical underpinnings

# we use RMSE as a metric because it is commonly used for regressions and because larger errors have a
# disproportionately large effect on RMSE, thus we are trying to reduce especially large errors



## --------------------------CONTROL-------------------------- ## 
# Specify how the models will be compared (cv
# for cross validation, 5 for 5 folds):
control = trainControl(method = "cv", number = 5, returnData = TRUE)



## --------------------------TUNING PARAMETERS-------------------------- ## 
# we tune the models based on the following parameters: note that this also gives 
# the algorithm the opportunity to behave differently for different sets of variables


# define tuning parameters:
n = 700  # number of boosting steps
m = c(4, 8)  # depth of boosted trees
e = c(0.01, 0.05)  # learning rate, common is < default of 0.3 to avoid overfitting
g = 0  # minimum loss reduction required to make a further partition on a leaf node
b = 0.8  # subsample ratio of columns when constructing a tree
w = 1  # minimum sum of instance weight: minimum number/weight of observations required to mke a further partition on a leaf node
s = 0.8  # random subsample of observations used to build the individual trees



xgb.parms = expand.grid(nrounds = n, max_depth = m, eta = e, gamma = g, colsample_bytree = b, min_child_weight = w, 
    subsample = s)



## --------------------------SET 1 XGB-------------------------- ##

## SET 1: All variables

# Use split sampling, so we can later assess the model's performance on the validation set:
idx1 = sample(1:nrow(set1), size = 0.8 * nrow(set1))
train1 = set1[idx1, ]
val1 = set1[-idx1, ]

# Train the model
xgb1 = train(price_doc ~ ., data = train1, method = "xgbTree", tuneGrid = xgb.parms, trControl = control, metric = "RMSE", 
    na.action = na.pass)


# Store the results:
xgb1_results = xgb1$results

# Best tuning parameters:
xgb1_tuning_RMSE = subset(xgb1_results, xgb1_results$RMSE == min(xgb1_results$RMSE))
xgb1_tuning_RMSE

# Prediction for validation set
xgb1.pred = predict(xgb1, newdata = val1, na.action = na.pass)

# Evaluate performance on validation set using RMSE (and R-Squared):
xgb1_performance = postResample(xgb1.pred, val1$price_doc)

# How wrong were we? In what direction?
xgb1_diff = xgb1.pred - val1$price_doc
summary(xgb1_diff)




## --------------------------SET 2 XGB-------------------------- ##

# based on variable importance of previously built xgb model, we select the following variables, where variable
# importance >= 0.5 (heuristic)

varImp = data.frame(varImp(xgb1)$importance)  # save importance measures
varImp$Vars = row.names(varImp)  # include variable names in new column
varImp_reduced = t(subset(varImp, varImp$Overall >= 0.5))  # transpose to have variables in columns

# set 2 reduces set 1 to important variables and the target variable:
set2 = set1[, names(set1) %in% varImp_reduced | names(set1) %in% c("price_doc")]
idx2 = sample(1:nrow(set2), size = 0.8 * nrow(set2))
train2 = set2[idx2, ]
val2 = set2[-idx2, ]
''
# Train the model
xgb2 = train(price_doc ~ ., data = train2, method = "xgbTree", tuneGrid = xgb.parms, trControl = control, metric = "RMSE", 
    na.action = na.pass)

# Store the results:
xgb2_results = xgb2$results

# Best tuning parameters:
xgb2_tuning_RMSE = subset(xgb2_results, xgb2_results$RMSE == min(xgb2_results$RMSE))
xgb2_tuning_RMSE

# Prediction for validation set
xgb2.pred = predict(xgb2, newdata = val2, na.action = na.pass)

# Evaluate performance on validation set using RMSE (and R-Squared):
xgb2_performance = postResample(xgb2.pred, val2$price_doc)

# How wrong were we? In what direction?
xgb2_diff = xgb2.pred - val2$price_doc
summary(xgb2_diff)



## --------------------------SET 3 XGB-------------------------- ##

# in the following we train a model on just a few of the variables we consider important to find out how much
# worse our predictions get


## SET 3:

set3_vars = c("full_sq", "area_meterprice", "kitch_sq", "material", "num_room", "rentincrease", "life_sq", "big_road_1km", 
    "bulvar_ring_km", "ID_railroad_station_avto", "month", "buildage", "month", "mortgage_rate", "deposits_growth", 
    "salary_growth", "gdp_deflator", "usdrub", "salary_growth", "pop_migration", "price_doc", "meterprice")

set3 = set1[, colnames(set1) %in% set3_vars]
idx3 = sample(1:nrow(set3), size = 0.8 * nrow(set3))
train3 = set3[idx3, ]
val3 = set3[-idx3, ]


# Train the model
xgb3 = train(price_doc ~ ., data = train3, method = "xgbTree", tuneGrid = xgb.parms, trControl = control, metric = "RMSE", 
    na.action = na.pass)


# Store the results:
xgb3_results = xgb3$results

# Best tuning parameters:
xgb3_tuning_RMSE = subset(xgb3_results, xgb3_results$RMSE == min(xgb3_results$RMSE))
xgb3_tuning_RMSE

# Prediction for validation set
xgb3.pred = predict(xgb3, newdata = val3, na.action = na.pass)

# Evaluate performance on validation set using RMSE (and R-Squared):
xgb3_performance = postResample(xgb3.pred, val3$price_doc)

# How wrong were we? In what direction?
xgb3_diff = xgb3.pred - val3$price_doc
summary(xgb3_diff)




## --------------------------PLS-------------------------- ## 

# run PLS on dataset3 (clean; standardized predictors) only:
set1 <- dataset3
idx1 = sample(1:nrow(set1), size = 0.8 * nrow(set1))
train1 = set1[idx1, ]
val1 = set1[-idx1, ]



# load required package
if (!require("pls")) install.packages("pls")
library("pls")

### PLS on all vars:

# PLS can easily be implemented using the Caret Package:
pls1 = train(train1[, !colnames(train1) %in% c("price_doc")], train1$price_doc, method = "pls", trControl = control, 
    tuneLength = 25)

# Prediction for validation set
yhat.pls1 = predict(pls1, newdata = val1)

# Evaluate model out-of-sample performance using RMSE (and R-Squared):
pls1_performance = postResample(yhat.pls1, val1$price_doc)



### PLS on hand-selected vars: PLS can easily be implemented using the Caret Package:
pls3 = train(train3[, !colnames(train3) %in% c("price_doc")], train3$price_doc, method = "pls", trControl = control, 
    tuneLength = 25)

# Prediction for validation set
yhat.pls3 = predict(pls3, newdata = val3)

# Evaluate model out-of-sample performance using RMSE (and R-Squared):
pls3_performance = postResample(yhat.pls3, val3$price_doc)
