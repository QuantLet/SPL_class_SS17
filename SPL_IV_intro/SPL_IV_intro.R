############### Instrumental Variable in simple linear regression

### Load Libraries

library(MASS)
library(ggplot2)
library(reshape2)
library(haven)
library(AER)
library(stargazer)

### Disable scientific notation

options(scipen = 999)

#### A simulated example

# We simulate x_1, x_2 and y from a multivariate normal distribution with a predetermined correlation structure

sd     = c(3, 5, 2)
cormat = cbind(c(1, 0.7, 0.6), c(0.7, 1, 0), c(0.6, 0, 1))
sigma  = (sd %*% t(sd)) * cormat
data   = mvrnorm(n = 100000, c(1, 0, 4), sigma)

x_1 = data[, 1]  # first variable
x_2 = data[, 2]  # second variable
z   = data[, 3]  # instrument

e = rnorm(100000, 0, 10)  # error term with sd 10

beta_0 = 5
beta_1 = 5
beta_2 = 1

y = beta_0 + beta_1 * x_1 + beta_2 * x_2 + e

# The estimator works well, we are close to the true covariates

summary(lm(y ~ x_1 + x_2))

# But if we cannot measure x_2 our coefficients are biased

summary(lm(y ~ x_1))

# and x_1 is correlated with the error term

cor(beta_2 * x_2 + e, x_1)

# we can solve this problem by using an instrumental variable which has to satisfy two conditions: (1) It has
# to be uncorrelated with the error term of our biased regression (2) It has to be correlated with the
# independent variable in the model whose coefficient is biased

cor(z, beta_2 * x_2 + e)
cor(z, x_1)

# We can empirically test (2) for the population given a sample by regressing x on z and checking the
# significance

summary(lm(x_1 ~ z))

# If our assumptions hold we can estimate the coefficient by

beta_1_hat = sum((z - mean(z)) * (y - mean(y)))/sum((z - mean(z)) * (x_1 - mean(x_1)))

# simplified:

beta_1_hat = cov(z, y)/cov(z, x_1)

# from there the intercept is obtained in the usual way:

beta_0_hat = mean(y) - beta_1_hat * mean(x_1)

# comparing the three models and printing them in a nice table

# Full Model

m1 = lm(y ~ x_1 + x_2)

# Omitted Variable OLS

m2 = lm(y ~ x_1)

# Instrumental Variable Estimator

m3 = ivreg(y ~ x_1 | z)


stargazer(m1, m2, m3)

#### A practical example

# We illustrate this by a small practical example: We extract data on married working women from our source

mroz = read_dta(url("http://www.uam.es/personal_pdi/economicas/rsmanga/docs/mroz.dta"))

# For comparison we first obtain the OLS estimates for the for the regression of logwage on education

summary(lm(lwage ~ educ, data = mroz))

# As an instrument we choose the education of the parent, which is correlated with education

cor(mroz$educ, mroz$fatheduc)
summary(lm(educ ~ fatheduc, data = mroz))

# Using fatheduc as an IV (utilizing the IV estimation command) from package AER) for educ gives

summary(ivreg(lwage ~ educ | fatheduc, data = mroz))

# The point estimate shrunk by almost 50%, though the SE greatly increased and the 95%-confidence interval
# still includes the old estimate

# creating the table

mroz1 = lm(lwage ~ educ, data = mroz)

mroz2 = ivreg(lwage ~ educ | fatheduc, data = mroz)


stargazer(mroz1, mroz2)
