#Install required packages
if(!require("haven")) install.packages("haven"); library("haven") #Read data.dta
if(!require("AER"))   install.packages("AER"); library("AER") #AER package for 2SLS
if(!require("Mass"))  install.packages("Mass"); library("Mass") #Gaussian ndim normal distruciton

##############
##Simulation##
##############
mi     = c(1, 0, 4) 
dev    = c(4, 7, 5)
c      = c(3,2,1,2,3,4,1,4,3)
varcov = matrix(c, nrow = 3, ncol = 3)
sg     = (dev %*% t(dev)) * varcov
data   = mvrnorm(n = 100000, mi, sg)
e      = rnorm(100000, mean = 0, sd= 1)

#Determine true coefficients
beta_0  = 2.0 
beta_1  = 0.7 
beta_2  = 0.6 
beta_3  = 0.8
beta_4  = 0.5
beta_5  = 0.5

#Determine x, z_3 and z_4
x      = data[, 1] #predictor of interest 
z_3    = data[, 2] #exogenous variable
z_4    = data[, 3] #exogenous variable
z_1    = rnorm(100000) #IV1
z_2    = rnorm(100000) #IV2


#First condition
#Cov of x and IV not equal 0
cov(x, z_1)
cov(x, z_2)
#The second condition will be tested (Sargan Test)


#Compute the simulated "true value of y
y = beta_0 + beta_1*x + beta_2*z_3 + beta_3*z_4 + e


############
#Endogenity#
############

#OLS and 2SLS
ols = lm(y ~ x + z_3 + z_4)
ts  = ivreg(y ~ x + z_3 + z_4 | z_1 + z_2 + z_3 + z_4 )

summary(ols)
summary(ts)

####################
### Hausman test ###
####################

#Hausmann Test for endogeneity
#The Hausman test detects whether differences in OLS and IV estimators are significant.Formally, it tests whether both OLS and IV estimator are consistant.
#As R packages do not include an explicit command for the Hausman exogenity test for all sorts of data, 
#we have programmed our own function considering the theoretical framework of the Hausman stasticist suggested by 
#Hausmann (1978). After having run both OLS "ols" and 2SLS "iv" regressions, this function either returns 
#the p-value and states whether the H0 was rejected,

#Hausman test function
#Initially, we perform the Hausman test to that tests joint consistency.
hausman       = function(ols, ts, alpha) {
  coef_diff   = coef(ts) - coef(ols) #Coefficient differences
  varcov_diff = vcov(ts) - vcov(ols) #Variance-covariance matrix
  xdiff       = as.vector(t(coef_diff) %*% solve(varcov_diff) %*% coef_diff) #Hausman test statistics
  pvalue      = pchisq(xdiff, df = 1, lower.tail = FALSE) #pvalue
  if(alpha>pvalue){
    cat("Based on the statistical test, differences are not significant with a pvalue of ", pvalue, "\n")
  }
  else{
    cat("Based on the statistical test, differences are not significant. The pvalue is", pvalue, "\n")
  }
}

#Example alpha=0.05
hausman(ols, ts, 0.05)

##########################
##### Wu-Hausman test ####
##########################
#The Wu-Hausman test for a single explanatory variable
#Structural equation: y = beta_0 + beta_1*x + beta_2*z_3 + beta_3*z_4 + e
#Where z_1 and z_2 are exogenous variables that are not included in the model
#x is the predictor of interest
#Reduced form of x: x = pi_0 + pi_1*z_1 + pi_2*z_2 + pi_3*z_3 + pi_4*z_4 + e_1 
#We regress x and keep the residual "res"

#Manual test
x_1 = lm(x ~ z_1 + z_2 + z_3 + z_4)
res = residuals(x_1)
#Run OLS on y = beta_0 + beta_1*x + beta_2*z_3 + beta_3*z_4 + delta*res + e_1
#Lastly, we run an OLS regression on y and test the residuals for significancy with a yoint F-test
coef = summary(lm(y ~ x + z_3 + z_4 + res))$coefficients

#In-built test
summary(ts, diagnostics = TRUE)$diagnostics

##########################
###### Sargan test #######
##########################
#In-built test
summary(ts, diagnostics = TRUE)$diagnostics

#Manual 
n        = 100000
ts       = ivreg(y ~ x + z_3 + z_4 | z_1 + z_2 + z_3 + z_4 )
res      = residuals(ts)
olsreg   = lm(res ~ z_1 + z_2 + z_3 + z_4 )
R2       = summary(olsreg)$r.squared #R^2
q        = 2 - 1 #Number of instruments (z_1 and z_2) - number of endogeneous predictors (here x) 
pvalue_r = pchisq(n*(summary(olsreg)$r.squared), q, lower.tail = FALSE) #Pvalue 









