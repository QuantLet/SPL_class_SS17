# Install and load packages
libraries = c("ggplot2", "xtable")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# Read in dataing Data:
data = read.csv("train.csv")


# Analysis of Target Variable
# Histogram of Target Variable
price.hist = ggplot(data, aes(x = SalePrice)) + geom_histogram(aes(y = ..density..), bins = 20, 
    colour = "black", fill = "white") + geom_density(alpha = 0.2, fill = "#FF6666") + ggtitle("Distribution of Sale Price vs. Normal Distribution") + 
    xlab("Sale Price") + stat_function(fun = dnorm, args = list(mean = mean(data$SalePrice, 
    na.rm = TRUE), sd = sd(data$SalePrice, na.rm = TRUE)), col = "blue", size = 2) + theme_classic()
ggsave(filename = "PriceHist.png")
# Histogram and QQ Plot of Log Prices
logprice.hist = ggplot(data, aes(x = log(SalePrice))) + geom_histogram(aes(y = ..density..), 
    bins = 20, colour = "black", fill = "white") + geom_density(alpha = 0.2, fill = "#FF6666") + 
    ggtitle("Distribution of  Log Sale Price vs. Normal Distribution") + xlab("Log Sale Price") + 
    stat_function(fun = dnorm, args = list(mean = mean(log(data$SalePrice), na.rm = TRUE), sd = sd(log(data$SalePrice), 
        na.rm = TRUE)), col = "blue", size = 2)  + theme_classic()
ggsave(filename = "LogPriceHist.png")

# Standardised Log Prices
stdprice.hist = ggplot(data, aes(x = (log(SalePrice) - mean(log(SalePrice), na.rm = TRUE))/sd(log(SalePrice), 
    na.rm = TRUE))) + geom_histogram(aes(y = ..density..), bins = 20, colour = "black", fill = "white") + 
    geom_density(alpha = 0.2, fill = "#FF6666") + ggtitle("Distribution of Standardised Log Sale Price vs. Normal Distribution") + 
    xlab("Standardised Log Sale Price") + stat_function(fun = dnorm, col = "blue", size = 2) + theme_classic()
ggsave(filename = "StdPriceHist.png")

stdprice.qq = ggplot(data, aes(sample = (log(SalePrice) - mean(log(SalePrice), na.rm = TRUE))/sd(log(SalePrice), 
    na.rm = TRUE))) + stat_qq() + geom_abline(slope = 1, intercept = 0, col = "red") + ggtitle("QQ-Plot of Standardised Log Sale Price") + 
    xlab("Standardised Log Sale Price")  + theme_classic()
ggsave(filename = "StdPriceQQ.png")

# Get Column Classes:
colclasses = sapply(data, class)
table(colclasses)

# Seperate data in numeric and categoric variables for further analysis
numeric.data = data[, names(colclasses[colclasses != "factor"])]
categoric.data = data[, names(colclasses[colclasses == "factor"])]

# Drop Id Variable since it is unnecessary here
numeric.data$Id = NULL
# Exclude Target Variable
numeric.data$SalePrice = NULL

# Define Functions for variable overview
getmode = function(x) {
    x = x[!is.na(x)]
    unique(x)[which.max(tabulate(match(x, unique(x))))]
}
getmodefreq = function(x) {
    mean(x == getmode(x), na.rm = TRUE)
}
getlevelcount = function(x) {
    x = x[!is.na(x)]
    length(unique(x))
}
#These are just wrappers around existing functions setting na.rm = TRUE
meanwrapper       = function(x) mean(x, na.rm = TRUE)
medianwrapper     = function(x) median(x, na.rm = TRUE)
sdwrapper         = function(x) sd(x, na.rm = TRUE)

# Create Overview table for categoric variables:
categoric.overview = data.frame(NACount = colSums(sapply(categoric.data, is.na)), LevelCount = sapply(categoric.data, 
    FUN = getlevelcount), Mode = sapply(categoric.data, FUN = getmode), ModeFrequency = sapply(categoric.data, 
    FUN = getmodefreq))
categoric.overview_latex = xtable(categoric.overview)

# Export Categorical Overview as Latex Table
rows.per.table = 30
row.indices = seq(from = 1, to = nrow(categoric.overview), by = rows.per.table)
latex.vector = character(0)
for(i in 1:length(row.indices)){
    cap = paste0("Overview Categorical Variables Table:",i)
    lab = paste0("tab:categoric.overview",i)
    categoric.overview_latex = xtable(categoric.overview[row.indices[i]:min(row.indices[i] + rows.per.table - 1, nrow(categoric.overview)),,drop = FALSE], caption = cap, label =lab)
    latex.vector = c(latex.vector, print(categoric.overview_latex))
}
all.latex = paste(latex.vector, collapse = "\n")
writeLines(all.latex, con = "categoric_overview.tex")


# Create Overview table for numericvariables Create Overview table for categoric variables:
numeric.overview = data.frame(NACount = colSums(sapply(numeric.data, is.na)), LevelCount = sapply(numeric.data, 
    FUN = getlevelcount), Mode = sapply(numeric.data, FUN = getmode), ModeFrequency = sapply(numeric.data, 
    FUN = getmodefreq), Mean = sapply(numeric.data, FUN = meanwrapper), Median = sapply(numeric.data, 
    FUN = medianwrapper), SD = sapply(numeric.data, FUN = sdwrapper))

# Export Categorical Overview as Latex Table
rows.per.table = 30
row.indices = seq(from = 1, to = nrow(numeric.overview), by = rows.per.table)
latex.vector = character(0)
for(i in 1:length(row.indices)){
    cap = paste0("Overview Numeric Variables Table:",i)
    lab = paste0("tab:numeric.overview",i)
    numeric.overview_latex = xtable(numeric.overview[row.indices[i]:min(row.indices[i] + rows.per.table - 1, nrow(numeric.overview)),,drop = FALSE], caption = cap, label =lab)
    latex.vector = c(latex.vector, print(numeric.overview_latex))
}
all.latex = paste(latex.vector, collapse = "\n")
writeLines(all.latex, con = "numeric_overview.tex")

