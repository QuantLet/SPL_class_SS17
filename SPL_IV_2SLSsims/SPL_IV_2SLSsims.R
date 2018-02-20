# Clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# Load packages
require(dplyr)
require(ggplot2)
require(MASS)
require(broom)
require(foreach)
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
# Simulation I: illustrating the degree of bias with increasing covariance of x1 with error
# -----------------------------------------------------------------------------------------

cat("Running simulation 1...")

# For reproducibility
set.seed(1234)

# vector of covariances of x1 and error term
cov_vec = seq(0, 1, length.out = 1000)

# run simulation using parallelized for-loop
model_df_corr = foreach(i = seq(cov_vec), .combine=rbind) %dopar% {

  models = twoStageSim(n_obs = 10000, cov_error = cov_vec[i], beta_z1 = 1.0, beta_z2 = 1.0)

}

# Save plot
path = "/Users/janekb/Documents/learn/iv/git/spl/instrumental_variables/32SLSsims"
jpeg(file = file.path(path, "sim_cov.jpeg"), width = 8.5, height = 6, units = 'in', res = 300)
model_df_corr %>%
  filter(term == "x1" | term == "x1_2sls") %>%
  ggplot(aes(x = cov_error, y = estimate, group = model, col = model)) +
  geom_line(aes(linetype = model)) + theme_classic() +
  labs(x = "Covariance between predictor and error term",
       y = "Estimate of predictor") +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks = seq(0, 1, 0.1))
dev.off()

# -----------------------------------------------------------------------------------------
# Simulation II: illustrating the consistency by increasing the sample size
# -----------------------------------------------------------------------------------------

cat("Running simulation 2...")

# vector of sample sizes
n_obs_vec = seq(1000, 1e6, 1000)

# run simulation using parallelized for-loop
model_df_nobs = foreach(i = seq(n_obs_vec), .combine=rbind) %dopar% {

  if(i %% 100 == 0){print(i)}
  models = twoStageSim(n_obs = n_obs_vec[i], cov_error = 0.5, beta_z1 = 1.0, beta_z2 = 1.0)

}

# save plot
jpeg(file = file.path(path, "sim_n.jpeg"), width = 8.5, height = 6, units = 'in', res = 300)
model_df_nobs %>%
  filter(term == "x1" | term == "x1_2sls") %>%
  ggplot(aes(x = n_obs, y = estimate, group = model, col = model)) +
  geom_line(aes(linetype = model)) + theme_classic() +
  labs(x = "Sample size",
       y = "Estimate of predictor") +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks =  seq(0, 1000000, 100000))
dev.off()


# -----------------------------------------------------------------------------------------
# Simulation III: highlighting the effect of instrument strength on standard error
# -----------------------------------------------------------------------------------------

# Instrument coefficients
beta_z1_vec = seq(1, 50, 1)
beta_z2_vec = seq(1, 50, 1)

model_df_weak_iv = foreach(i = seq(beta_z1_vec), .combine=rbind) %dopar% {

  models = twoStageSim(n_obs = 10000, cov_error = 0.5, beta_z1 = beta_z1_vec[i], beta_z2 = beta_z2_vec[i])

}

# save plot
jpeg(file = file.path(path, "sim_weak_iv.jpeg"), width = 8.5, height = 6, units = 'in', res = 300)
model_df_weak_iv %>%
  filter(term == "x1" | term == "x1_2sls") %>%
  filter(model == "2SLS") %>%
  ggplot(aes(x = beta_z1, y = std.error)) +
  geom_line() + theme_classic() +
  labs(x = "Strength of instrument",
       y = "Estimate of standard error for x1",
       title = "") +
  theme(legend.title = element_blank())
dev.off()
