#Name of Quantlet:  SPL_TSAnalysis
#Published in:      Statistical Programming Languages
#Description:       Predicting periods of decline of Moscow's housing market through relevant Google Trends, such as the search keyword 'real estate market' or
#                   or the complete category 'real estate'.
#Keywords:          google trends, forecasting, time series, housing market
#Author:            David Berscheid
#Submitted:         Sun, Jun 25 2017





rm(list=ls())
graphics.off()


## --------------------------Packages & Libraries-------------------------- ##
libraries = c("fpp", "graphics","tframePlus","xts","mFilter","quantmod","hydroGOF","dplyr", "dynlm")
lapply(libraries,function(x)if(!(x %in% installed.packages())){install.packages(x)})
lapply(libraries,require,quietly=TRUE,character.only=TRUE)
source("customFunctions.R")






#Housing Index    
#import data, extract relevant information & transform it into a time series
index = read.csv("officialindex.csv", header=T, sep = ",")
index = subset(index, Indicator == "Real house price indices, s.a." )          
index = index$Value
dat = ts(index, frequency = 4, start = c(2004,1)) #set quarterly and start quarter
plot(dat)








## --------------------------Data Import-------------------------- ##
#google category trends
google1 = read.csv("cat1.csv", header = F, sep = ",")
google2 = read.csv("cat2.csv", header = F, sep = ",")
google3 = read.csv("cat4.csv", header = F, sep = ",")
google4 = read.csv("cat5.csv", header = F, sep = ",")
#google keyword trend
keywordtrend = read.csv("keywordtrend.csv", header = T, sep = ";")


## --------------------------Preprocessing--------------------------- ##
#rearanging data frames

google1 = google1 %>% 
  slice(3:n()) %>% 
  rename(date = V1) %>% 
  rename(trend1 = V2)

google2 = google2 %>% 
  slice(3:n()) %>% 
  rename(date = V1) %>% 
  rename(trend2 = V2)

google3 = google3 %>% 
  slice(3:n()) %>% 
  rename(date = V1) %>% 
  rename(trend3 = V2)

google4 = google4 %>% 
  slice(3:n()) %>% 
  rename(date = V1) %>% 
  rename(trend4 = V2)

trendword1 = keywordtrend$trend1


#merging the dataframes
trend = cbind(google1, google2, google3, google4, trendword1)

#use custom function for checking, if the structure is correct 
strCheck(dataframe = trend)

trend$date = NULL
trend$date = NULL
trend$date = NULL

strCheck(dataframe = trend)


#converting to time series objects, clarifying monthly data, beginning in 2004, first quarter
t1 = ts(trend$trend1, frequency = 12, start = c(2004,1)) 
t2 = ts(trend$trend2, frequency = 12, start = c(2004,1))
t3 = ts(trend$trend3, frequency = 12, start = c(2004,1))
t4 = ts(trend$trend4, frequency = 12, start = c(2004,1))
trendword1 = ts(trend$trendword1, frequency = 12, start = c(2004,1)) 

#plot of time series untreated  
ts.plot(t1, t2,t3, t4,trendword1,
        gpars=list(xlab="time", ylab="trendvalue", lty=c(1:4)))



ts.plot(t2,t3, t4,
        gpars=list(xlab="time", ylab="trendvalue", lty=c(1:4)))

cor(t2,t3) 
cor(t2,t4) 
cor(t3,t4) 


#convert monthly data into quarterly data by taking the mean of 3 months 
t1 = na.omit(as.quarterly(t1, FUN=mean, na.rm=FALSE))
t2 = na.omit(as.quarterly(t2, FUN=mean, na.rm=FALSE))
t3 = na.omit(as.quarterly(t3, FUN=mean, na.rm=FALSE))
t4 = na.omit(as.quarterly(t4, FUN=mean, na.rm=FALSE))
trendword1 = na.omit(as.quarterly(trendword1, FUN=mean, na.rm=FALSE))

## --------------------------Time Series Analysis--------------------------- ##
#removing seasonality
#visually inspecting time series and characteristics of additive or multiplicative
plot(t1)
#t1 -> looks multiplicative
t1decom = decompose(t1, "multiplicative")
plot(t1decom)
t1s = t1 / t1decom$seasonal


#t1: multiplicative time series 
t1decom = decompose(t1, "multiplicative")
#remove seasonal effects
t1s = t1 / t1decom$seasonal


plot(t2)
#t2 -> looks multiplicative
t2decom = decompose(t2, "multiplicative")
plot(t2decom)
t2s = t2 / t2decom$seasonal

plot(t3)
#t3 -> looks multiplicative
t3decom = decompose(t3, "multiplicative")
plot(t3decom)
t3s = t3 / t3decom$seasonal

plot(t4)
#t4 -> looks multiplicative
t4decom = decompose(t4, "multiplicative")
plot(t4decom)
t4s = t4 / t4decom$seasonal

plot(trendword1)
#t1 -> looks multiplicative
trendword1decom = decompose(trendword1, "multiplicative")
plot(trendword1decom)
trendword1s = trendword1 / trendword1decom$seasonal



#Detrending with Hodrick-Prescott-Filter
t1cyclic = hpfilter(t1s)
t1cyclic = t1cyclic$cycle
# plot(hpfilter(t1s))

t2cyclic = hpfilter(t2s)
t2cyclic = t2cyclic$cycle
# plot(hpfilter(t2s)) 

t3cyclic = hpfilter(t3s)
t3cyclic = t3cyclic$cycle
# plot(hpfilter(t3s))  

t4cyclic = hpfilter(t4s)
t4cyclic = t4cyclic$cycle
# plot(hpfilter(t4s))  


trendword1cyclic = hpfilter(trendword1s)
trendword1cyclic = trendword1cyclic$cycle
# plot(hpfilter(trendword1s))


#Detrending the inde with Hodrick-Prescott-Filter
datfilter = hpfilter(dat)
datcyclic = datfilter$cycle
# plot(hpfilter(dat))


#Resolving autocorrelation 
#lags for index variable:
auto.arima(datcyclic, ic = "aic", seasonal = FALSE)
#ARIMA(1,0,3) with zero mean     
#so include late one lag 

auto.arima(t1cyclic, ic = "aic", seasonal = FALSE)
auto.arima(t2cyclic, ic = "aic", seasonal = FALSE) # AR 1 
auto.arima(t3cyclic, ic = "aic", seasonal = FALSE) # AR 1 
auto.arima(t4cyclic, ic = "aic", seasonal = FALSE) # ARIMA 2 0 2 


#Investigating autocorrelation with acf function & Augmented Dickey Fuller Tests
#for google trend 1
acf(t1cyclic, lag.max=4)
t1lag2 = diff(t1cyclic ,lag = 2) #lag order: 2
acf(t1lag2, lag.max=4) #successful

#for google trend 2
acf(t2cyclic, lag.max=4)
t2lag2 = diff(t2cyclic ,lag = 2) #lag order: 2
acf(t2lag2, lag.max=4) #successful


#for google trend 3
acf(t3cyclic, lag.max=4)
t3lag2 = diff(t3cyclic ,lag = 2) #lag order: 2
acf(t3lag2, lag.max=4) #successful



#for google trend 4
acf(t4cyclic, lag.max=10) #no lags necessary

#for google trend 3
acf(trendword1cyclic, lag.max=4)
tw1lag1 = diff(trendword1cyclic ,lag = 1) #lag order: 1
acf(tw1lag1, lag.max=4) #successful

#prediction model
fit <- dynlm(datcyclic ~ L(t1cyclic,2)+  L(t2cyclic,3)   + L(trendword1cyclic,1) + L(datcyclic,1))
summary(fit)



# Time series regression with "ts" data:
#   Start = 2004(4), End = 2016(4)
# 
# Call:
#   dynlm(formula = datcyclic ~ L(t1cyclic, 2) + L(t2cyclic, 3) + 
#           L(trendword1cyclic, 1) + L(datcyclic, 1))
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -18.4299  -1.6120   0.3625   1.8022   7.1080 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            -0.22917    0.55928  -0.410  0.68397    
# L(t1cyclic, 2)         -0.11401    0.08404  -1.357  0.18183    
# L(t2cyclic, 3)          0.20005    0.09818   2.037  0.04764 *  
#   L(trendword1cyclic, 1)  0.37924    0.12426   3.052  0.00384 ** 
#   L(datcyclic, 1)         0.80153    0.07096  11.296 1.37e-14 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.88 on 44 degrees of freedom
# Multiple R-squared:  0.7887,	Adjusted R-squared:  0.7695 
# F-statistic: 41.07 on 4 and 44 DF,  p-value: 2.567e-14

