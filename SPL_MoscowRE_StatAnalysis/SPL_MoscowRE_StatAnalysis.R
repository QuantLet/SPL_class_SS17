# Name of Quantlet: SPL_StatAnalysis 
# Published in: Statistical Programming Languages
# Description: Conducting a statistical analysis, heteroskedasticity and normality checks on
# housing data provided by Sberbank 
# Keywords: normality, jarque-bera-test,
# heteroskedasticity, transformation, squared norm 
# Author: Alisa Kolesnikova 
# Submitted: Fr,
# Aug 18 2017


rus = readRDS("clean_set")
str(rus)

## --------------------------Packages & Libraries-------------------------- ##

if (!require("tseries")) install.packages("tseries")
library("tseries")
if (!require("lmtest")) install.packages("lmtest")
library("lmtest")



## ---------Examining (the distribution of) price_doc and transformations---------- ##

# The preset target variable is price_doc. Let's examine it
plot(rus$price_doc)

# Delete couple of extreme values:
which.max(rus$price_doc)
rus = rus[rus$price_doc < 8e+07, ]
rus1 = rus[,-376] # price_doc as target var, thus deleting meterprice
rus2 = rus[,-265] # meterprice as target var, thus deleting price_doc

# Examining normality:
hist(rus$price_doc)  # right-skewed, we therefore consider a log or square root transformation
hist(log(rus$price_doc))  # looks more normal
hist(sqrt(rus$price_doc))  # looks more normal

# Let's add the normal distribution line for visual comparison:
x = log(rus$price_doc)
h = hist(x, breaks = 40, col = "grey", xlab = "Log of price_doc", main = "Histogram with Normal Curve")
xfit = seq(min(x), max(x), length = 40)
yfit = dnorm(xfit, mean = mean(x), sd = sd(x))
yfit = yfit * diff(h$mids[1:2]) * length(x)
lines(xfit, yfit, col = "blue", lwd = 2)

z = sqrt(rus$price_doc)
hz = hist(z, breaks = 40, col = "grey", xlab = "Square root of price_doc", main = "Histogram with Normal Curve")
zfit = seq(min(z), max(z), length = 40)
tfit = dnorm(zfit, mean = mean(z), sd = sd(z))
tfit = tfit * diff(hz$mids[1:2]) * length(z)
lines(zfit, tfit, col = "blue", lwd = 2)



# Kolgomorov Smirnov Test with Lilliefors modification (since we are estimating mean and
# variance) to check for normality of target variable price_doc / log(price_doc) /
# sqrt(price_doc):

if (!require("nortest")) install.packages("nortest")
library("nortest")
lillie.test(rus$price_doc)
lillie.test(log(rus$price_doc))
lillie.test(sqrt(rus$price_doc))
# The test statistic is smallest in case of sqrt transformation -> closest to normal
# distribution



# Jarque Bera Tests for rus$price_doc, log(rus$price_doc) and sqrt(rus$price_doc)
jarque.bera.test(rus$price_doc)
jarque.bera.test(log(rus$price_doc))
jarque.bera.test(sqrt(rus$price_doc))

# Let's also look at the price per meter (meterprice)
hist(rus$meterprice)
jarque.bera.test(rus$meterprice)  # not too good either



# in case of linear regression, it would probably be advisable to use a square root
# transformation of the target variable. However, such a transformation is not relevant in
# case of eXtreme Gradient Boosting, but will be analyzed in Models.R



## --------------------------Analysing residuals in case of linear
## regression-------------------------- ##


# Examinig the set in regression, without meterprice(289) or without price_doc (286): We
# therefore have to delete one of the target variables first, respectively:
rus1$meterprice = NULL
rus2$price_doc = NULL
lm1 = lm(price_doc ~ ., data = rus1)
summary(lm1)

# Adjusted R-squared: 0.623, a lot of important variables. However 15 coef not defined due to
# several singularities.

# Check residuals using a jarque.bera.test and visualizations:
u.hat1 = resid(lm1)
jarque.bera.test(u.hat1)  #residuals are not evenly distributed
hist(u.hat1, freq = F)
min = min(u.hat1)
max = max(u.hat1)
mu = mean(u.hat1)
sigma = sd(u.hat1)
curve(dnorm(x, mu, sigma), min, max, add = T)



# Checking for heteroskedasticity:

# Graphical examination
par(mfrow = c(2, 2))  # init 4 charts in 1 panel
plot(lm1)
# Heteroschedasticity seems to be present as on the graph with Resid vs Fitted there is a
# triangle-like pattern with a big curve. Also, on the Scale-Location Graph one might see a
# very uneven distribution along the red line.

# Apply Breusch-Pagan Test:
bptest(lm1)
# studentized Breusch-Pagan test

# data: lm1 BP = 5102.6, df = 309, p-value < 2.2e-16 variance of the residuals is obviously
# not constant, so the graphical guess was right - we would have to account for
# heterosedasticity in case of linear regression

# We could try a Box-Cox transformation or rather use machines learning methods alternative
# to regressions in order to rectify the heteroskedasticity.
