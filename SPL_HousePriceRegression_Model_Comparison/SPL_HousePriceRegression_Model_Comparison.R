# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# Install and load packages
libraries = c("ggplot2", "xtable", "gridExtra", "gbm", "plyr", "glmnet", "randomForest")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})

lapply(libraries, library, quietly = TRUE, character.only = TRUE)


# Read in pre-processed Data:
train = read.csv("train_preprocessed.csv")
test  = read.csv("test_preprocessed.csv")

# Read in trained models
load("rf.RData")                     # random forest
load("gbm.RData")                    # gradient boosting machine
load("regression_models_fit.RData")  # OLS based models


# set rownumbers in dataframe to NULL
train$X  = NULL
train$Id = NULL
test$X   = NULL
test$Id  = NULL

# Comparing the models by mean squared error (MSE)
model.mse = function(model, test.data = test) {
    if (class(model)[1] %in% c("train", "lm")) {
        pred = predict(model, newdata = test.data)
        mse  = (1/nrow(test.data)) * sum((pred - test.data$logSalePrice)^2)
    } else {
        pred = predict(model, newx = as.matrix(test.data[!names(test.data) %in% "logSalePrice"]), s = "lambda.1se")
        mse  = (1/nrow(test.data)) * sum((pred - test.data$logSalePrice)^2)
    }
    return(mse)
}

# Comparing the models by MSE on the training data
model_overfit.mse = function(model, test.data = train) {
    if (class(model)[1] %in% c("train", "lm")) {
        pred = predict(model, newdata = test.data)
        mse  = (1/nrow(test.data)) * sum((pred - test.data$logSalePrice)^2)
    } else {
        pred = predict(model, newx = as.matrix(test.data[!names(test.data) %in% "logSalePrice"]), s = "lambda.1se")
        mse  = (1/nrow(test.data)) * sum((pred - test.data$logSalePrice)^2)
    }
    return(mse)
}


# Comparing the models by mean absolute errror
model.mae = function(model, test.data = test) {
    if (class(model)[1] %in% c("train", "lm")) {
        pred = predict(model, newdata = test.data)
        mae  = (1/nrow(test.data)) * sum(abs(pred - test.data$logSalePrice))
    } else {
        pred = predict(model, newx = as.matrix(test.data[!names(test.data) %in% "logSalePrice"]), s = "lambda.1se")
        mae  = (1/nrow(test.data)) * sum(abs(pred - test.data$logSalePrice))
    }
    return(mae)
}

# Comparing the models by bias
model.bias = function(model, test.data = test) {
    if (class(model)[1] %in% c("train", "lm")) {
        pred = predict(model, newdata = test.data)
        bias = (1/nrow(test.data)) * sum(pred - test.data$logSalePrice)
    } else {
        pred = predict(model, newx = as.matrix(test.data[!names(test.data) %in% "logSalePrice"]), s = "lambda.1se")
        bias = (1/nrow(test.data)) * sum(pred - test.data$logSalePrice)
    }
    return(bias)
}

# Comparing the models by R^2
model.Rsq = function(model, test.data = test) {
    if (class(model)[1] %in% c("train", "lm")) {
        pred = predict(model, newdata = test.data)
        Rsq  = 1 - sum((test.data$logSalePrice - pred)^2)/sum((test.data$logSalePrice - mean(test.data$logSalePrice))^2)
    } else {
        pred = predict(model, newx = as.matrix(test.data[!names(test.data) %in% "logSalePrice"]), s = "lambda.1se")
        Rsq  = 1 - sum((test.data$logSalePrice - pred)^2)/sum((test.data$logSalePrice - mean(test.data$logSalePrice))^2)
    }
    return(Rsq)
}

# comparing of the models using different measures
model.list                  = list(lm.fit, fwd.fit, lasso.fit, ridge.fit, gbmtuned, rftuned)
comparisonMSE.list          = sapply(model.list, FUN = model.mse)
comparisonMSEtrain.list     = sapply(model.list, FUN = model_overfit.mse)
comparisonMAE.list          = sapply(model.list, FUN = model.mae)
comparisonBIAS.list         = sapply(model.list, FUN = model.bias)
comparisonRSQ.list          = sapply(model.list, FUN = model.Rsq)
comparison.result           = matrix(c(round(comparisonMSE.list, 3), round(comparisonMSEtrain.list, 3), round(comparisonMAE.list, 3), 
    round(comparisonBIAS.list, 3), round(comparisonRSQ.list, 3)), ncol = length(model.list), byrow = TRUE)
rownames(comparison.result) = c("MSE", "MSEtrain", "MAE", "BIAS", "RSQ")
colnames(comparison.result) = c("bwd", "fwd", "lasso", "ridge", "gbm", "rf")
comparison.result

# Writing a latex table containing the model comparison results
modelcomparison_latex = xtable(comparison.result, digits = 3, caption = "Performance measures for all implemented models", label = "tab:measures")
print(modelcomparison_latex, file = "modelcomparison.tex", table.placement = "H")


# Plotting the real values in the test dataset against the estimations 

# Making predictions for every model on the test data
predictions.bwd   = predict(lm.fit, newdata = test)                        
predictions.fwd   = predict(fwd.fit, newdata = test)
predictions.lasso = predict(lasso.fit, newx = as.matrix(test[!names(test) %in% "logSalePrice"]), s = "lambda.1se")
predictions.ridge = predict(ridge.fit, newx = as.matrix(test[!names(test) %in% "logSalePrice"]), s = "lambda.1se")
predictions.gbm   = predict(gbmtuned, newdata = test)
predictions.rf    = predict(rftuned, newdata = test)

# Running an OLS regression of real values on predicted for the plots
predicted.values           = data.frame(cbind(predictions.bwd,predictions.fwd,predictions.lasso,predictions.ridge,predictions.gbm,predictions.rf))
colnames(predicted.values) = c("predictions.bwd","predictions.fwd","predictions.lasso","predictions.ridge","predictions.rf","predictions.gbm")
coeff.lm                   = vector("list", ncol(predicted.values)) # preparing an empty list for coefficients of regression

# Looping through all the predictions of the different models
for (i in 1:ncol(predicted.values)){
    data.temp = data.frame(cbind(test$logSalePrice,predicted.values[,i]))
    lm.temp = lm(data.temp[,1]~data.temp[,2])
    coeff.lm[[i]] = coef(lm.temp)
}


# plot real values against estimated values lm.fit plot
df.bwd.fit  = data.frame(cbind(test$logSalePrice, predictions.bwd ))  # creating dataframe containing real and predicted outcome

bwd.plot    = ggplot(df.bwd.fit, aes(predictions.bwd,V1)) + geom_point() + geom_segment(x = -4, y = -4, xend = 4, yend = 4, color = "red", size = 1.3) +
    stat_smooth(method = "lm", se = FALSE) + labs(title = "Backward selection linear model", x = "bwd.fit predictions", y = "logSalePrice") +
    annotate("text", label = paste("int:", round(coeff.lm[[1]][1],4), sep = " "), x = -2, y = 3, size =10, color = "blue") +
    annotate("text", label = paste("slope:", round(coeff.lm[[1]][2],4), sep = " "), x = -2, y = 2.5, size =10, color = "blue") + 
    theme_classic(base_size = 20)

# fwd.fit plot
df.fwd.fit  = data.frame(cbind(test$logSalePrice, predictions.fwd ))  # creating dataframe containing real and predicted outcome

fwd.plot    = ggplot(df.fwd.fit, aes(predictions.fwd,V1)) + geom_point() + geom_segment(x = -4, y = -4, xend = 4, yend = 4, color = "red", size = 1.3) +
    stat_smooth(method = "lm", se = FALSE) + labs(title = "Forward selection linear model", x = "fwd.fit predictions", y = "logSalePrice") +
    annotate("text", label = paste("int:", round(coeff.lm[[2]][1],4), sep = " "), x = -2, y = 3, size =10, color = "blue") +
    annotate("text", label = paste("slope:", round(coeff.lm[[2]][2],4), sep = " "), x = -2, y = 2.5, size =10, color = "blue") + 
    theme_classic(base_size = 20)

# lasso.fit plot
df.lasso.fit  = data.frame(cbind(test$logSalePrice, predictions.lasso ))  # creating dataframe containing real and predicted outcome

lasso.plot    = ggplot(df.lasso.fit, aes(predictions.lasso,V1)) + geom_point() + geom_segment(x = -4, y = -4, xend = 4, yend = 4, color = "red", size = 1.3) +
    stat_smooth(method = "lm", se = FALSE) + labs(title = "Lasso regression model", x = "lasso.fit predictions", y = "logSalePrice") +
    annotate("text", label = paste("int:", round(coeff.lm[[3]][1],4), sep = " "), x = -2, y = 3, size =10, color = "blue") +
    annotate("text", label = paste("slope:", round(coeff.lm[[3]][2],4), sep = " "), x = -2, y = 2.5, size =10, color = "blue") + 
    theme_classic(base_size = 20) 

# ridge.fit plot
df.ridge.fit  = data.frame(cbind(test$logSalePrice, predictions.ridge ))  # creating dataframe containing real and predicted outcome

ridge.plot    = ggplot(df.ridge.fit, aes(predictions.ridge,V1)) + geom_point() + geom_segment(x = -4, y = -4, xend = 4, yend = 4, color = "red", size = 1.3) +
    stat_smooth(method = "lm", se = FALSE) + labs(title = "Ridge regression model", x = "ridge.fit predictions", y = "logSalePrice") +
    annotate("text", label = paste("int:", round(coeff.lm[[4]][1],4), sep = " "), x = -2, y = 3, size =10, color = "blue") +
    annotate("text", label = paste("slope:", round(coeff.lm[[4]][2],4), sep = " "), x = -2, y = 2.5, size =10, color = "blue") + 
    theme_classic(base_size = 20) 

# gbm plot
df.gbm  = data.frame(cbind(test$logSalePrice, predictions.gbm ))  # creating dataframe containing real and predicted outcome

gbm.plot    = ggplot(df.gbm, aes(predictions.gbm,V1)) + geom_point() + geom_segment(x = -4, y = -4, xend = 4, yend = 4, color = "red", size = 1.3) +
    stat_smooth(method = "lm", se = FALSE) + labs(title = "Gradient Boosting Machine", x = "gbmtuned predictions", y = "logSalePrice") +
    annotate("text", label = paste("int:", round(coeff.lm[[5]][1],4), sep = " "), x = -2, y = 3, size =10, color = "blue") +
    annotate("text", label = paste("slope:", round(coeff.lm[[5]][2],4), sep = " "), x = -2, y = 2.5, size =10, color = "blue") + 
    theme_classic(base_size = 20)

# rf plot
df.rf  = data.frame(cbind(test$logSalePrice, predictions.rf ))  # creating dataframe containing real and predicted outcome

rf.plot    = ggplot(df.gbm, aes(predictions.rf,V1)) + geom_point() + geom_segment(x = -4, y = -4, xend = 4, yend = 4, color = "red", size = 1.3) +
    stat_smooth(method = "lm", se = FALSE) + labs(title = "Random Forest", x = "rftuned predictions", y = "logSalePrice") +
    annotate("text", label = paste("int:", round(coeff.lm[[6]][1],4), sep = " "), x = -2, y = 3, size =10, color = "blue") +
    annotate("text", label = paste("slope:", round(coeff.lm[[6]][2],4), sep = " "), x = -2, y = 2.5, size =10, color = "blue") + 
    theme_classic(base_size = 20)


# saving resulting graphs individually 
plot.list      = list(bwd.plot, fwd.plot, lasso.plot, ridge.plot, gbm.plot, rf.plot) # list of all graphs
names.plotting = c("bwd_fit", "fwd_fit", "lasso_fit", "ridge_fit", "gbmtuned","rftuned")

for (i in 1:length(names.plotting)){
    png(paste(names.plotting[i],".png",sep=""), units = "in", res = 72, height = 8.27, width = 11.69)
    plot(plot.list[[i]])
    dev.off()
}

