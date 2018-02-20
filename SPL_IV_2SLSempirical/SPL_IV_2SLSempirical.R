# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# load packages
require(foreign)
require(MASS)
require(stargazer)
require(AER)

# load data directly from Dartmouth college's econ20 website
mroz = read.dta("http://www.dartmouth.edu/~econ20pa/mroz.dta")

# validate data source (based on expected types)
is.integer(mroz$inlf)
is.numeric(mroz$lwage)
is.integer(mroz$exper)
is.integer(mroz$expersq)
is.integer(mroz$fatheduc)
is.integer(mroz$motheduc)

# Exclude cases where inlf == 1 (Wooldridge example focuses on women in labor force)
mroz = subset(mroz, inlf == 1)

# Testing the subsetting result (hours should be positive for every subject)
all(mroz$hours > 0)

# Generate expected output using 2SLS implementation from AER package
m_validate = ivreg(lwage ~ educ + exper + expersq | motheduc + fatheduc + exper + expersq, data = mroz)

# Test validity of instruments
summary(m_validate, diagnostics = TRUE)$diagnostics # "Weak instruments" should be significant

# Pretty summary using stargazer
stargazer(summary(m_validate, diagnostics = TRUE)$diagnostics[1, ], type = "text", digits = 3)

## Example 15.5, p. 530
# Biased estimate
fml1 = as.formula("lwage ~ educ + exper + expersq")
m1   = lm(fml1, data = mroz)

# First stage
fml2 = as.formula("educ ~ exper + expersq + motheduc + fatheduc")
m2   = lm(fml2, data = mroz)
mroz$educ_2SLS <- predict(m2)

# 2SLS
fml3 = as.formula("lwage ~ educ_2SLS + exper + expersq")
m3   = lm(fml3, data = mroz)

# Compare results from biased and 2SLS model
stargazer(m1, m3, type = "text",
          dep.var.labels=c("log(wage)"))

# Validate results
if(all(round(coef(m3), 10) == round(coef(m_validate), 10))){
  cat("Manually computed coefficients match AER package results.")
}else{
  cat("Manually computed coefficients do not match AER package results!")
}
