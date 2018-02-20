############### Properties of the IV-estimator

### Load Libraries

library(MASS)
library(ggplot2)
library(reshape2)
library(stargazer)

### Disable scientific notation

options(scipen = 999)

# We explore three properties of the IV-estimator

#### Consistency

# The IV-estimator is in fact consistent and converges to the true value we show this by simulation using
# subsequently larger values of n and contrast this result to the full OLS model as well as the omitted
# variable OLS

n = c(seq(2, 10, 0.5) %o% 10^(1:6))  # the vector of sample sizes

sd     = c(3, 5, 2)  # the SDs of the variables
cormat = rbind(c(1, 0.7, 0.6), c(0.7, 1, 0), c(0.6, 0, 1))  # the correlation matrix
sigma  = (sd %*% t(sd)) * cormat  # the covariance matrix

estimate = sapply(n, function(x) {
    # apply the following function to each element in n and return the result
    print(paste("n =", x, "..."))  # for denoting progress
    
    data_sim = mvrnorm(n = x, c(1, 0, 4), sigma)  # drawing data from a multivariate normal w covariance sigma
    x_1_sim  = data_sim[, 1]
    x_2_sim  = data_sim[, 2]
    z_sim    = data_sim[, 3]
    
    e_sim = rnorm(n = x, 0, 10)  # error term with SD = 10
    
    beta_0_sim = 5  # coefficients
    beta_1_sim = 5
    beta_2_sim = 1
    
    y_sim = beta_0_sim + beta_1_sim * x_1_sim + beta_2_sim * x_2_sim + e_sim  # true model
    
    beta_1_hat_iv          = cov(z_sim, y_sim)/cov(z_sim, x_1_sim)  # IV-estimator for beta_1
    beta_1_hat_ols         = lm(y_sim ~ x_1_sim + x_2_sim)$coefficients[2]  # OLS-estimator for beta_1
    beta_1_hat_ols_incompl = lm(y_sim ~ x_1_sim)$coefficients[2]  # Incomplete OLS for beta_1
    
    return(c(beta_1_hat_iv, beta_1_hat_ols, beta_1_hat_ols_incompl))
})



# plot

consistency        = cbind.data.frame(n, t(estimate))  # creating data frame containing all info for plotting
names(consistency) = c("n", "IV", "OLS", "OLS_omitted")  # renaming variables

ggplot(data = melt(consistency, id.vars = "n", variable.name = "Estimation")) + geom_hline(yintercept = 5, color = "grey") +
    geom_line(aes(x = n, y = value, color = Estimation, linetype = Estimation)) + scale_x_log10(name = "Sample Size",
    breaks = c(1 %o% 10^(1:7))) + scale_y_continuous(name = "Estimated Coefficient") + scale_linetype_discrete(name = "Estimation Procedure",
    labels = c("IV", "OLS", "OLS (omitted Covariate)")) + scale_color_discrete(guide = "none") + theme_classic()

ggsave("consistency.png", width = 14, height = 7)

#### Bad instruments

# We consider three times three cases: an instrument which is not(0.0)/moderately(0.1)/strongly(0.5) correlated
# with the error term and highly(0.6)/moderately(0.4)/barely(0.1) correlated with the instrumented variable


cases = expand.grid(error_corr = c(0, 0.1, 0.5), var_corr = c(0.6, 0.4, 0.1))  # different correlation combinations

n = c(seq(2, 10, 0.5) %o% 10^(1:6))  # the vector of sample sizes

mse = sapply(n, function(x) {
    # apply the following function to each element in n and return the result
    
    print(paste("n =", x, "..."))  # for denoting progress
    
    # generates a sample for each correlation structure with n observations
    
    all_cases <- lapply(1:dim(cases)[1], n = x, function(i, n) {
        
        cormat = rbind(c(1, 0.7, cases[i, 2]), c(0.7, 1, cases[i, 1]), c(cases[i, 2], cases[i, 1], 1))  # the correlation matrix
        sd     = c(3, 5, 2)  # the SDs of the variables
        sigma  = (sd %*% t(sd)) * cormat
        
        data = mvrnorm(n = n, c(1, 0, 4), sigma)
        
        return(data)
        
    })
    
    # estimates for all given cases the coefficients
    
    estimates_all_cases <- sapply(all_cases, n = x, function(k, n) {
        
        x_1_sim = k[, 1]
        x_2_sim = k[, 2]
        z_sim   = k[, 3]
        
        e_sim = rnorm(n = n, 0, 10)  # error term with SD = 10
        
        beta_0_sim = 5  # coefficients
        beta_1_sim = 5
        beta_2_sim = 1
        
        y_sim = beta_0_sim + beta_1_sim * x_1_sim + beta_2_sim * x_2_sim + e_sim  # true model
        
        beta_1_hat = cov(z_sim, y_sim)/cov(z_sim, x_1_sim)  # IV-estimator for beta_1
        
        return(beta_1_hat)
    })
    
    
    mse = (estimates_all_cases - 5)^2  # calculates mse
    
    return(mse)
    
})


mse_data         = cbind.data.frame(n, t(mse))  # creating data frame containing all info for plotting
names(mse_data)  = c("n", "hvar_lerr", "hvar_merr", "hvar_herr", "mvar_lerr", "mvar_merr", "mvar_herr", "lvar_lerr", 
    "lvar_merr", "lvar_herr")  # renaming variables
plot_data        = melt(mse_data, id.vars = "n", variable.name = "Correlation")
plot_data        = cbind(plot_data, matrix(unlist(strsplit(as.character(plot_data$Correlation), "_")), ncol = 2, byrow = TRUE))[, 
    -2]  # splits names from above to create to columns for ggplot
names(plot_data) = c("n", "estimate", "var_corr", "err_corr")  # again renaming 

ggplot(data = plot_data) + geom_hline(yintercept = 5, color = "grey") + geom_line(aes(x = n, y = estimate, color = var_corr, 
    linetype = err_corr)) + geom_point(aes(x = n, y = estimate, shape = var_corr, color = var_corr), alpha = 0.6) + 
    scale_x_log10(name = "Sample Size", breaks = c(1 %o% 10^(1:7))) + scale_y_log10(name = "Estimated MSE") + theme_classic() + 
    scale_shape_discrete(name = "Correlation between Instrument \n and instrumented Variable", labels = c("r = 0.6", 
        "r = 0.1", "r = 0.4")) + scale_color_discrete(guide = "none") + scale_linetype_discrete(name = "Correlation between Instrument \n and Error Term", 
    labels = c("r = 0.5", "r = 0.0", "r = 0.1"))
ggsave("bad_instruments.png", width = 14, height = 7)
