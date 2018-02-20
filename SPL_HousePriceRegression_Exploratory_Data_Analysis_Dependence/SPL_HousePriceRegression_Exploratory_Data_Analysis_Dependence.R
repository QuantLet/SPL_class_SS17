# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# Install and load packages
libraries = c("ggplot2", "xtable", "corrplot", "gridExtra")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})

lapply(libraries, library, quietly = TRUE, character.only = TRUE)


# Read in training Data:
data = read.csv("train.csv")

# Get Column Classes:
colclasses = sapply(data, class)

# Seperate data in numeric and categoric variables for further analysis
numeric.data   = data[, names(colclasses[colclasses != "factor"])]
categoric.data = data[, names(colclasses[colclasses == "factor"])]

# Drop Id Variable since it is unnecessary here
numeric.data$Id = NULL


# Part1: 
# Analysing correlations for numeric variables

# function, that creates a correlation plot 
corr.func = function(data, cut.value, corr.mat = FALSE, corr.test = FALSE, significance = 0.05) {
    corr.numeric = cor(na.omit(numeric.data))               # produces correlation matrix of all numeric variables in the dataset
    
    # find columns of data, which have correlations higher then cut.value
    find.rows = apply(corr.numeric, 1, function(x) sum(abs(x) > abs(cut.value)) > 1)
    
    # subset correlation matrix for plotting
    corr.numeric.adjusted = corr.numeric[find.rows, find.rows]
    
    # find data, which has low correlation
    low.corr = colnames(corr.numeric) %in% colnames(corr.numeric.adjusted)
    cat("The variables", "\n", paste0(colnames(corr.numeric)[!low.corr], collapse = ", "), "\n", "have very low bivariate correlations with the other numeric variables in the training data set!")
    
    # test correlations at certain significance level using a function, that produces a p-value matrix for all bivariate correlations
    correlation.test = function(corr.data) {
        corr.data            = as.matrix(corr.data)
        n                    = ncol(corr.data)
        p.value.matrix       = matrix(NA, n, n)
        diag(p.value.matrix) = 0
        
        for (i in 1:(n - 1)) {
            for (j in (i + 1):n) {
                tmp                  = cor.test(corr.data[, i], corr.data[, j]) # testing correlation
                p.value.matrix[i, j] = p.value.matrix[j, i] = tmp$p.value       # filling p-value matrix with respective p-values
            }
            colnames(p.value.matrix) = rownames(p.value.matrix) = colnames(corr.numeric.adjusted)
        }
        return(p.value.matrix)
    }
    
    
    # save resulting correlation matrix
    png("Corrplot.png")
    if (corr.test == FALSE) {
        corrplot(corr.numeric.adjusted, method = "square", tl.col = "black")
    } else {
        corrplot(corr.numeric.adjusted, p.mat = correlation.test(corr.numeric.adjusted), sig.level = significance, 
            method = "square", tl.col = "black")
    }
    dev.off()
    
    # print raw correlation matrix if desired
    if (corr.mat == TRUE) 
        return(corr.numeric.adjusted)
}

corr.func(numeric.data, cut.value = 0.3, corr.mat = FALSE, corr.test = TRUE)



# Part 2:
# Ordered barplot of the bivariate correlations between the target variable SalePrice and numeric variables in
# the dataset

corr.barplot = function(numb.corr) {
    if (numb.corr > ncol(numeric.data) - 1) {
        return("Warning: You can choose at most all numeric variables in the dataset except the target variable SalePrice, which is already implemented as default value")
    } else {
        correlation.vars    = names(numeric.data) %in% c("SalePrice")
        correlation.data    = numeric.data[!correlation.vars]                  # subsetting the numeric variables to not contain the target variable
        correlations        = vector(length = length(names(correlation.data))) # setting up vector for results (correlations)
        names(correlations) = names(correlation.data)
        
        for (i in names(correlation.data)) {                                   # calculating all bivariate correlations
            correlations[i] = cor(numeric.data$SalePrice, correlation.data[i], use = "pairwise.complete.obs")
        }
        
        # setting up the results for plotting
        y.plotting        = correlations[order(abs(correlations), decreasing = TRUE)][1:numb.corr]
        x.plotting        = names(y.plotting)
        names(y.plotting) = NULL
        df                = data.frame(x.plotting, y.plotting)
        df$x.plotting     = factor(df$x.plotting, levels = df[order(abs(df$y.plotting), decreasing = TRUE), "x.plotting"])
        
        ggplot(data = df, aes(x.plotting, y.plotting), fill = as.factor(x.plotting)) + geom_bar(stat = "identity", color = "black", fill = "black") + 
            theme(panel.background = element_rect(fill = "white", colour = "black"),axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, size = 12)) + 
            ylab("Correlation") + ggtitle(paste("Barplot of the", numb.corr, "highest bivariate correlations with SalePrice", 
            sep = " "))
    }
}

# plotting the barplot in RStudio
corr.barplot(36)

# saving the plot as PNG
path.barplot = file.path(getwd(), "Barplot_ordered.png")
png(file = path.barplot)
corr.barplot(20)
dev.off()



# Part 3: 
# Analysing relations between catagorical explanatory variables and target variable

# Creating a function, that produces boxplots of the target variable SalePrice in relation to the levels 
# of all categorical variables in the dataset
boxplot.target = function(categoric) {
    if (class(data[, categoric]) != "factor") {
        return("The input variable has to be categorical. Numeric input does not work!")
    } else {
        # datapreparation for boxplots
        categoric.x            = data[, categoric]
        plot.data              = as.data.frame(cbind(data$SalePrice, categoric.x))
        plot.data[[2]]         = as.factor(plot.data[[2]])
        levels(plot.data[[2]]) = levels(categoric.x)
        
        # creating the boxplot
        ggplot(plot.data, aes(x = categoric.x, y = V1)) + geom_boxplot() + labs(title = paste("Boxplots of SalePrice", 
            "\n", "depending on", categoric, sep = " "), x = categoric, y = "SalePrice") + theme_classic()
    }
}



boxplot.list = list()  # setting up empty list to save results of boxplot function

first.variable = 1     # position of first variable to use 
last.variable  = 6     # position of last variable to use

# filling boxplot.list with the results of boxplot.target function and saving results as pdf
for (j in 1:7){
    for (i in first.variable:last.variable) {
        plotting.variable = colnames(categoric.data[i])
        boxplot.loop      = boxplot.target(plotting.variable)
        boxplot.list      = c(boxplot.list, list(boxplot.loop))
      }
    # saving the plots as PNG
    path = file.path(getwd(), paste("boxplot_", first.variable, "through", last.variable, ".png", sep = ""))
  
    png(file = path)
    do.call("grid.arrange", c(boxplot.list, ncol = 2))
    dev.off()
    boxplot.list = list()
    
    # changing variables to be used
    first.variable = first.variable + 6
    if(j == 6){last.variable = last.variable + 7}else{last.variable = last.variable + 6}
}


# printing the plots in RStudio
boxplot.list   = list()
first.variable = 1     # position of first variable to use 
last.variable  = 6     # position of last variable to use

for (i in first.variable:last.variable) {
    plotting.variable = colnames(categoric.data[i])
    boxplot.loop      = boxplot.target(plotting.variable)
    boxplot.list      = c(boxplot.list, list(boxplot.loop))
}

do.call("grid.arrange", c(boxplot.list, ncol = 2))





