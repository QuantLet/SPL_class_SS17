#clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

#install and load packages
libraries = c("xtable", "outreg", "glmnet", "ggplot2", "ggfortify")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#read in data: Please set your working directory!
df    = read.csv("train_preprocessed.csv")
#set rownumbers in dataframe to NULL
df$X  = NULL
df$Id = NULL

#1. Linear Model 
#function, that starts with all vars and keeps only significant ones until every var is
#significant
sign.select = function(dframe, y) {
    pvals          = 1
    z              = 1
    i              = 1
    vars.selection = names(dframe)
    vars.selection = vars.selection[!vars.selection %in% y]
    while (z > 0) {
        df.lm          = cbind(dframe[vars.selection], dframe[y])
        lm1            = lm(formula(paste(y, "~ . ")), data = df.lm)
        pvals          = summary(lm1)$coefficients[, 4]
        pvals          = pvals[!names(pvals) %in% "(Intercept)"]
        vars.selection = names(pvals[pvals < 0.05])
        z              = sum(pvals > 0.05)
        print(vars.selection)
        i = i + 1
        if (i == 300) {
             warning("Did not finish in 300 iterations. No significant variables in data set?")
             break
        }
     }
     return(vars.selection)
}

vars   = sign.select(df, "logSalePrice")
lm.fit = lm(logSalePrice ~ ., data = df[, c(vars, "logSalePrice")])
summary(lm.fit)

#2. forward stepwise regression based on AIC 
all.fit  =  formula(lm(logSalePrice ~ ., data = df))
none.fit = lm(logSalePrice ~ 1, data = df)
fwd.fit  = step(none.fit, direction='forward', scope=all.fit, trace=TRUE)

#plot the AIC values vs. No. Variables
AIC = fwd.fit$anova$AIC
VAR = row_number(-AIC)
gg  = as.data.frame(cbind(VAR, AIC))

png(file = "step.png", width = 16, height = 16, unit='cm', res = 100)
ggplot(aes(x=VAR, y = AIC), data = gg)+ geom_point() + theme_classic() 
dev.off()

# 3. Ridge and Lasso
#vars for ridge and lasso
y = as.matrix(df$logSalePrice)
x = as.matrix(df[!names(df) %in% c("logSalePrice")])

#ridge and lasso regression function to estimate the optimal penalty parameter lambda
#with 10-fold cross validation
lm.penal = function(type, x, y) {
    if (type == "lasso") {
        alpha = 1
    } else if ( type == "ridge") {
        alpha = 0
    } else
        stop("type must be either ridge or lasso")
    cvfit          = cv.glmnet(x, y, alpha = alpha, nfolds = 10)
    fit            =  predict(cvfit,newx=x, s="lambda.1se")
    sst            = sum(y^2)
    sse            = sum((fit - y)^2)
    # R squared
    rsq            = 1 - sse / sst
    c              = coef(cvfit, s = "lambda.1se")
    inds           = which(c != 0)
    variables      = row.names(c)[inds]
    vars.selection = variables[!variables %in% "(Intercept)"]
    coeftable      = data.frame(var = variables,
                                coeff            = c[inds],
                                stringsAsFactors = FALSE)
    c              = round(c, digits = 3)
    rsq            = round(rsq, digits = 2)
    output         = list(vars.selection, coeftable, c, cvfit, fit, rsq)
}

#perform regressions
lasso = lm.penal(type = "lasso", x = x, y = y)
ridge = lm.penal(type = "ridge", x = x, y = y)

#plot the optimal lamdba for lasso
png(file = "lasso_lambda.png", width = 16, height = 16, unit='cm', res = 100)
autoplot(lasso[[4]]) + theme_classic() + theme(panel.background = element_rect(fill='white', color="black"))
dev.off()

#plot the lasso penalty results
lasso.plot = lm.penal(type="lasso", x = as.matrix(df[, lasso[[1]]]), y = y)
png(file = "lasso.png", width = 24, height = 16, unit='cm', res = 100)
autoplot(lasso.plot[[4]]$glmnet.fit, xvar="lambda") + theme_classic() 
dev.off()

#make a table for latex
#dummy table that includes all vars
dummy     = lm(logSalePrice~., data=df)
table.out = outreg(setNames(list(lm.fit, fwd.fit, dummy, dummy), c("Sign. Selec.", "AIC Selec.", "Lasso", "Ridge")), se = FALSE)
#cut away unnecessary stats
table.out = table.out [1:180,]
#replace dummycoeffs
table.out[,"Lasso"]     = as.character(c(as.vector(lasso[[3]]), nrow(df), lasso[[6]]))
table.out[,"Ridge"]     = as.character(c(as.vector(ridge[[3]]), nrow(df), ridge[[6]]))
table.out[table.out==0] = ""
table.out               = rbind(table.out, c("", "Number Vars", length(lm.fit$coefficients),  length(fwd.fit$coefficients), length(lasso[[1]]), 
                               length(ridge[[1]])))
table.x                 = xtable(table.out[c(1:7, 13:19, 177:181), ], caption = "Excerpt of Regression Model Results")
print(table.x, type = "latex", file = "reg_table.tex",include.rownames = FALSE)

#save R objects for further analysis
lasso.fit = lasso[[4]]
ridge.fit = ridge[[4]]
objects   = c("lasso.fit", "ridge.fit", "lm.fit", "fwd.fit")
save(list = objects, file = "regression_models_fit.RData")