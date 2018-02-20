# Clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# Load packages
require(dplyr)
require(ggplot2)
require(MASS)
require(broom)
require(foreach)
library(parallel)
require(doMC)
registerDoMC(parallel::detectCores()) # Register all cores for parallelized computation

# -----------------------------------------------------------------------------------------
# Define simulation function
# -----------------------------------------------------------------------------------------

twoStageSim = function(n_obs = NA, cov_error = NA, beta_z1 = NA, beta_z2 = NA){
  
  # Simulate the joint distribution of x*, x2 and c
  n_obs   = n_obs         # number of observations
  mu      = c(20, 15, 10) # vector of means for x1, x2 and c
  cov12   = 0.0           # covariance between x1 and x2
  cov13   = cov_error     # covariance between x1 and error (as c is unobserved, it moves into the error term)
  cov23   = 0.0           # covariance between x2 and error (x2 is exogenous)
  sigma   = matrix(c(1, cov12, cov13,
                     cov12, 1, cov23,
                     cov13, cov23, 1), 3, 3)
  data    = mvrnorm(n = n_obs, mu = mu, Sigma = sigma)
  
  # Simulate the instruments
  x1Star = data[, 1] # true x1
  x2     = data[, 2] # x2
  c      = data[, 3] # unobserved covariate that's correlated with xStar
  z1     = rnorm(n_obs)
  z2     = rnorm(n_obs)
  x1     = x1Star + beta_z1 * z1 + beta_z2 * z2 # observed/endogenous x
  
  # Simulate outcome y
  beta_0  = 1.0 # Intercept
  beta_x1 = 0.7 # weight for x1
  beta_x2 = 0.6 # weight for x2
  beta_c  = 0.9 # weight for c
  eps     = rnorm(n = n_obs, mean = 0, sd = 1) # Standard normal error term
  y       = beta_0 * 1 + beta_x1 * x1 + beta_x2 * x2 + beta_c * c + eps
  
  # Estimate effect of x on y
  m_true   = lm(y ~ 1 + x1 + x2 + c) # if c were known
  m_biased = lm(y ~ 1 + x1 + x2)     # if we ignore c
  
  # Recover true effect of x on y using 2SLS estimation
  x1_2sls   = fitted(lm(x1 ~ 1 + z1 + z2 + x2)) # x1_2sls is the instrument for x
  m_2SLS = lm(y ~ 1 + x1_2sls + x2)
  
  # Collect results
  models           = rbind(tidy(m_true), tidy(m_biased), tidy(m_2SLS))
  models$cov_error = cov_error
  models$n_obs     = n_obs
  models$beta_z1   = beta_z1
  models$beta_z2   = beta_z2
  models$model     = c(rep("True model", 4), rep("Biased model", 3), rep("2SLS", 3))
  
  return(models)
}

# -----------------------------------------------------------------------------------------
# Performance comparison between foreach and mclapply
# -----------------------------------------------------------------------------------------

# For using foreach and mclapply, only one argument can vary. Therefore, I choose to fix everything but n_obs.
twoStageSim2 <- function(n){return(twoStageSim(n_obs = n, cov_error = 0.5, beta_z1 = 10, beta_z2 = 10))}

# Options for simulations
set.seed(1235)
n_min = 100
n_max = 100000
n_by  = 100
n_obs_vec = seq(n_min, n_max, n_by)

# lapply with 1 core
lapply_1core = system.time({
  
  plyr::rbind.fill(lapply(n_obs_vec, twoStageSim2))
  
})

# mclapply with 2 cores
mclapply_2cores = system.time({
  
  plyr::rbind.fill(mclapply(n_obs_vec, twoStageSim2, mc.cores = 2))
  
})

# mclapply with 4 cores
mclapply_4cores = system.time({
  
  plyr::rbind.fill(mclapply(n_obs_vec, twoStageSim2, mc.cores = 4))
  
})

# foreach with 2 cores
registerDoSEQ() # first, go back to sequential computation
registerDoMC(2) # register 2 cores for foreach

# run simulation using parallelized for-loop
foreach_2cores = system.time({
  foreach(i = seq(n_obs_vec), .combine=rbind) %dopar% {
    
    models = twoStageSim2(n = n_obs_vec[i])
    
  }
})

# foreach with 4 cores
registerDoSEQ()
registerDoMC(4)

foreach_4cores = system.time({
  foreach(i = seq(n_obs_vec), .combine=rbind) %dopar% {
    
    models = twoStageSim2(n = n_obs_vec[i])
    
  }
})

performance = data.frame(method = c("lapply",
                                    "mclapply with 2 cores", 
                                    "mclapply with 4 cores", 
                                    "foreach with 2 cores",
                                    "foreach with 4 cores"), 
                         n_min = rep(100, 5), n_max = rep(10000, 5), 
                         number_of_samples = rep(100, 5), 
                         time_in_sec = c(as.numeric(lapply_1core['elapsed']),
                                         as.numeric(mclapply_2cores['elapsed']),
                                         as.numeric(mclapply_4cores['elapsed']),
                                         as.numeric(foreach_2cores['elapsed']),
                                         as.numeric(foreach_4cores['elapsed'])
                         ))

library(ggplot2)
jpeg("sim_performance.jpeg", width = 8.5, height = 6, units = 'in', res = 300)
ggplot(performance, aes(x = method, y = time_in_sec, col = method)) + geom_point(size = 3) +
  theme_minimal() + labs(x = "Method", y = "Elapsed time in seconds") +
  scale_y_continuous(limits = c(0, 300)) +
  theme(legend.title = element_blank())
dev.off()
