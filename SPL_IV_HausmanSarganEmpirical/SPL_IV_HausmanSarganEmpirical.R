#Install required packages
if(!require("haven")) install.packages("haven"); library("haven") #Read data.dta
if(!require("AER"))   install.packages("AER")  ; library("AER") #AER package for 2SLS

#########################
## A practical example ##
#########################
#Load the data on married working women.
mroz = read_dta(url("http://www.uam.es/personal_pdi/economicas/rsmanga/docs/mroz.dta"))

#Itinitally, we reduce our data set to women in the labor force
mroz = subset(mroz, inlf == 1)

#Run a OLS and 2SLS estimation. 
ols  = lm(lwage ~ educ + exper + expersq, data = mroz) #OLS estimation

#Instrumental variables "fatheduc" and "motheduc

#Run a 2SLS estimation. "fatheduc" and "motheduc" are instrumental variable of "educ". 
ts   = ivreg(lwage ~ educ + exper + expersq | exper + expersq + motheduc + fatheduc, data = mroz )

#OLS and 2SLS results
r_1  = summary(ols)
r_2  = summary(ts)

####################
### Hausman test ###
####################
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

#Run Hausman test
hausman(ols, ts, 0.05)

#The test will be conduced on a significance level of 5 %. 
"Based on the statistical test, differences are not significant. The pvalue is 0.1006218."

#########################
#### Wu-Hausman test ####
#########################
#The simplest and most accurate way to perform the Wu-Hausmann endogeneity test is to extract information 
#from the summary including "diagnostics" argument from the "AER" package. 
summary(ts, diagnostics= TRUE)

#Pvalue
pvalue_1 = summary(ts, diagnostics= TRUE)$diagnostics[2,4]


###############
#Manual coding#
###############
#Estimate residual from reduced form of our predictor of interest "educ"
olseduc  = lm(educ ~ exper + expersq + motheduc + fatheduc, data = mroz)
#Save estimated residuals
res      = residuals(olseduc)
#Add "res" as a predictor to the structural equation.
ols_2    = lm(lwage ~ educ + exper + expersq + res, data = mroz)
summary(ols_2)
#Pvalue
pvalue_2 = summary(ols_2)$coefficients[5,4]

#################################################
#### Testing Overidentification Restrictions ####
#################################################
#As "educ" has two IVs "motheduc" and "fatheduc", we are associated with multiple IVs for a single variable. 
#Testing whether the instrumental variables are correlated with the error term

#We run an 2SLS regression 
ts = ivreg(lwage ~ educ + exper + expersq | exper + expersq + motheduc + fatheduc, data = mroz )

#One overidentifying restriction, as "educ" has two IVs "motheduc" and "fatheduc"
#Likewise to the Wu-Hausman test, the Sargan test is also included in the "diagonstics" argument of the "AER" package 
summary(ts, diagnostics= TRUE)

#As the Sargan test is included in the AER package, results are the following:
pvalue_3 = summary(ts, diagnostics= TRUE)$diagnostics[3,4] #pvalue
#Based on the Sargan test, a high p-value leads to insignificant results. The instrumental
#variables parents' education are not significantly correlated with the error term. 
#Due to the joint significance test included in the "AER" package, the manual coding of the test for each single variable is not necessary.  
