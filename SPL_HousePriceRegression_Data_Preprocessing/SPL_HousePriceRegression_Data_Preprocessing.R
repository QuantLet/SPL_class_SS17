# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# Install and load packages
libraries = c("ggplot2", "reshape2", "dplyr")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#read in data: Please set your working directory!
df = read.csv("train.csv")
#delete ID and take logs of sale price, see quantlet explanatory data analysis
df$logSalePrice = log(df$SalePrice)
df$SalePrice    = NULL
df$Id           = NULL

# 1. Missings and Imputation table with the number of NAs per Variable
na.summary = function(data) {
    na_summary = sapply(data, function(x) sum(is.na(x)))
    na_summary[na_summary > 0]
}
na.summary(df)

# some NA's do have a meaning, though
vars = c("Alley", "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2", 
    "FireplaceQu", "GarageType", "GarageFinish", "GarageQual", "GarageCond", "PoolQC", 
    "Fence", "MiscFeature")
# change NA to None
na2none = function(data, var) {
    levels(data[, var]) = c(levels(data[, var]), "none")
    data[, var][is.na(data[, var])] = "none"
    return(data[, var])
}

for (i in 1:length(vars)) {
    df[, vars[i]] = na2none(df, vars[i])
}

# Check the results of above function
na.summary(df)

# Plot a missingness map. Again the na.summary function
# comes in handy
na_vars = names(na.summary(df))
# make a function for a missingness plot with ggplot2
miss.plot = function(x) {
    x     %>% 
    is.na %>% 
    melt  %>% 
    ggplot(data = ., aes(x = Var2, y = Var1))  + geom_raster(aes(fill = value))   + scale_fill_discrete(name = "", labels = c("Present", "Missing"))    + theme_classic()     + theme(axis.text.x = element_text(angle = 45, vjust = 0.5))  + labs(x = "Variables in Dataset", y = "Rows / observations")
}

png(file = "missmap.png", width = 16, height = 16, unit='cm', res = 100)
miss.plot(df[na_vars])
dev.off()

# Select categorical and numerical variables
colclasses     = sapply(df[na_vars], class)
categoric.data = df[, names(colclasses[colclasses == "factor"])]
numeric.data   = df[, names(colclasses[colclasses != "factor"])]

# Impute numeric data with median
impute.median = function(x) {
    nas    = is.na(x)
    x[nas] = median(x[!nas])
    as.numeric(x)
}

numeric.imputed = as.data.frame(sapply(numeric.data, impute.median))

# imputation for the categoric data we need a custom mode function, because R's built
# in works only for numeric data
Mode = function(x) {
    ux = unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

impute.mode = function(x) {
    nas    = is.na(x)
    x[nas] = Mode(x[!nas])
    as.factor(x)
}

categoric.imputed = as.data.frame(sapply(categoric.data, impute.mode))

# make new dataframe with non-imputed colums from df, and the imputed variables
df.imputed = cbind(df[, !(names(df) %in% names(cbind(categoric.imputed, numeric.imputed)))], 
                   categoric.imputed, numeric.imputed)

# check if all missings are gone
anyNA(df.imputed)

# 2. merge factorlevels that are almost empty function to set categories with less
# than 20 observations to 'other'
single.factors = function(data) {
    for (var in names(data)) {
        if (is.factor(data[[var]])) {
            tbl = table(data[[var]])
            ren = names(tbl)[tbl <= 20]
            # rename all matching levels to other
            levels(data[[var]])[levels(data[[var]]) %in% ren] = "Other"
            # same procedure again, if now there is still a category with less 
            # than 20 it can only be "other"!
            tbl     = table(data[[var]])
            tbl_sum = sum(tbl < 20)
            if (nlevels(data[[var]]) < 3 & tbl_sum >= 1) 
                data[[var]] = NA
            }
      }
      return(data)
}

# apply function and remove the NAs
df.merged = single.factors(df.imputed)
df.merged = df.merged[, colSums(is.na(df.merged)) != nrow(df.merged)]

# 3. Outliers find outliers in numeric data
colclasses      = sapply(df.merged, class)
df.temp.numeric = df.merged[, names(colclasses[colclasses != "factor"])]

# Make the function to identify extreme outliers (more than 3 times IQR)
outlier.count = function(x) {
    low  = as.numeric(quantile(x)[2] - IQR(x) * 3)
    high = as.numeric(IQR(x) * 3 + quantile(x)[4])
    sum(x >= high | x <= low)
}

# make a table to see the count of outliers
outlier_table = sapply(df.temp.numeric, outlier.count)
outlier_table = outlier_table[outlier_table > 0]
outlier_table

# removing vars with IQR = 0 (the case where all vars are 'outliers'), because these
# variables will cause trouble wenn we split the dataset. also, 0 means that something
# doesnt exist which is coverd by the other vars
outlier_table[outlier_table == 1460]
df.temp.numeric[names(outlier_table[outlier_table == 1460])] = NULL

# function to return the number of unique values of a var
unique.values = function(var) {
    un = unique(var)
    length(un)
}

count_unique = sapply(df.temp.numeric, unique.values)
# vars with less than 10 distinct values, are they sensibly numeric?
less_than_10 = count_unique[count_unique < 10]
sapply(df.temp.numeric[names(less_than_10)], table)
# it looks OK!

# now cut the remaining outliers to 3.0 IQR distance
outlier.truncate = function(x) {
    low         = as.numeric(quantile(x)[2] - IQR(x) * 3)
    high        = as.numeric(IQR(x) * 3 + quantile(x)[4])
    x[x < low]  = low
    x[x > high] = high
    print(x)
    return(x)
}

df.outlier.trunc = as.data.frame(sapply(df.temp.numeric, outlier.truncate))

# make a number of boxplots for some vars with many outliers for comparision 
df.plot           = rbind(df.temp.numeric[, names(outlier_table[outlier_table<1460& outlier_table>5])], df.outlier.trunc[, names(outlier_table[outlier_table<1460& outlier_table>5])])
df.plot$truncated = as.factor(c(rep(0, nrow(df.temp.numeric)), rep(1, nrow(df.outlier.trunc))))
gg                = melt(df.plot, id="truncated")

png(file = "boxplots.png", width = 24, height = 16, unit='cm', res = 100)
ggplot(data = gg, aes(x=variable, y=value)) + geom_boxplot(aes(fill=truncated))  + facet_wrap( ~ variable, scales="free")  + theme_classic() 
dev.off()

# 4. Plot the cleaned dataset make a number of histograms
gg = melt(scale(df.outlier.trunc[, !names(df.outlier.trunc) %in% c("Id")]))

png(file = "histograms.png", width = 24, height = 16, unit='cm', res = 100)
qplot(data = gg, x = value, facets = ~Var2, bins = 30) + theme_classic() 
dev.off()

# 5. Reduction of dimensionality of numeric variables only select those vars, that
# have a reasonable correlation
cormat = cor(df.outlier.trunc[, !names(df.outlier.trunc) %in% c("logSalePrice")])
list   = which(abs(cormat) > 0.7 & abs(cormat) < 1, arr.ind = TRUE)

df.pca = df.outlier.trunc[unique(rownames(list))]
cor(df.pca)

# principal component analysis
prin1 = princomp(df.pca, cor = T, scores = T)
png(file = "screeplot.png", width = 16, height = 16, unit='cm', res = 100)
screeplot(prin1, type = "l")
dev.off()
summary(prin1)

# we use 4 components, to explain > 70% of variance
scores = prin1$scores[, 1:4]

#to be able to select factor levels automatically, convert all factors into dummies
mm = model.matrix(~. - 1, data = df.merged[, names(colclasses[colclasses == "factor"])])

# make dataframe from remaining numeric vars and PCA-Results and dummy vars
df.preprocessed = cbind(df.outlier.trunc[, !names(df.outlier.trunc) %in% rownames(list)], scores, mm)

#avoid spaces in names
df.preprocessed        = as.data.frame(df.preprocessed)
names(df.preprocessed) = make.names(names(df.preprocessed), unique = TRUE)

#standardize numerics
df.preprocessed[!names(df.preprocessed) %in% colnames(mm)]= scale(df.preprocessed[!names(df.preprocessed) %in% colnames(mm)])

summary(df.preprocessed)

#split into test and trainingsdata
df.preprocessed$Id = 1:nrow(df.preprocessed)
#make 80/20 split
set.seed(123)
ids = sample(df.preprocessed$Id, nrow(df.preprocessed)*0.2)

write.csv(df.preprocessed[df.preprocessed$Id %in% ids,], file = "test_preprocessed.csv")
write.csv(df.preprocessed[!df.preprocessed$Id %in% ids,], file = "train_preprocessed.csv")