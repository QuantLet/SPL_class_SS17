# Using this script, we load the raw and messy datasets, which will then be used in the models quantlet
# We refrain from putting this in the Models script just to keep it tidier.



# dataset 1: messy data 

# merge train and macro data sets by timestamp
# and delete the transaction ID variable

traincsv = read.csv('train.csv', sep=',')
macrocsv = read.csv('macro.csv', sep=',')
dataset1 = merge(traincsv, macrocsv, by='timestamp')
dataset1$timestamp = as.Date(dataset1$timestamp)
dataset1$id = NULL

 

# dataset 2: clean data

# read previously cleaned data
# delete meterprice variable, which had been used to construct area_meterprice
# cut off extraordinary high values of the target variable (it improved accuracy a lot), i.e. > 75%-Quantile + 1.5*IQR (heuristic)
# and again, delete the transaction ID variable
dataset2 = readRDS('Data_Clean', refhook=NULL)
dataset2$meterprice = NULL
dataset2$id = NULL
options(scipen=10)
#boxplot(dataset2$price_doc)

i25 = as.numeric(summary(dataset2$price_doc)[2])
i75 = as.numeric(summary(dataset2$price_doc)[5])
maximale = i75 + 1.5 * (i75 - i25)
dataset2$price_doc[dataset2$price_doc>maximale] = maximale 






# dataset 3: clean data and standardised predictors

# adapt dataset 2 by standardising numeric predictors
dataset3 = dataset2

standardise = function(var) {
  mu = mean(var)
  sd = sd(var)
  result = (var - mu) / sd
  return(result)
}
idx.num = sapply(dataset3, is.numeric)
dataset3[,idx.num & !colnames(dataset3) %in% c('price_doc')] = lapply(dataset3[,idx.num & !colnames(dataset3) %in% c('price_doc')], standardise)




# dataset 4: 

# has been adapted directly in the model script by
# changing target in train() function to sqrt(price_doc) and adapting prediction by using ^2, 
# e.g. xgb.pred  = predict(xgb, newdata = val)^2 to then be able to compare the RMSE value to the other models
