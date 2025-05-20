# setup -------------------------------------------------------------------

library(nimble)
library(ggplot2)
library(MCMCvis)
library(bayesplot)

# source simulation function
source("scripts/simulate_occupancy_data.R")

# simulate data -----------------------------------------------------------

sim <- simulate_occupancy_data(
  seed = 1988,
  grid_cell_size = 5,
  total_days = 60,
  sampling_interval = 7,
  zeta = 2,
  spread_center = c(10, 75),
  detect_spatial = FALSE
  # inhomogeneous = TRUE,
  # n_sites = 20
)

# True underlying occupancy
sim$plots$occupancy
# Observations
sim$plots$detection

# model code --------------------------------------------------------------

occupancy_code <- nimbleCode({
  
  beta0 ~ dnorm(0, sd = 2)
  zeta ~ dgamma(5, 2.5)
  
  phi ~ dbeta(8, 2)
  alpha0 ~ dnorm(0, sd = 3)
  # alpha1 ~ dnorm(0, sd = 1)
  psi1 ~ dbeta(1, 1)
  
  x_prime ~ dunif(0, 100)
  y_prime ~ dunif(0, 100)
  
  for (s in 1:S) {
    dist_to_center[s] <- sqrt((x_coord[s] - x_prime)^2 + (y_coord[s] - y_prime)^2)
    distances[s] <- -dist_to_center[s]
    logit(p_s[s]) <- alpha0 # + alpha1 * dist_to_center[s]
  }
  
  for (s in 1:S) {
    z[s, 1] ~ dbern(psi1)
    for (t in 2:T) {
      rho[s, t] <- t + (1 / zeta) * distances[s]
      logit(gamma[s, t]) <- beta0 + rho[s, t]
      muZ[s, t] <- z[s, t - 1] * phi + (1 - z[s, t - 1]) * gamma[s, t]
      z[s, t] ~ dbern(muZ[s, t])
    }
  }
  
  for (s in 1:S) {
    for (t in 1:T) {
      for (j in 1:J) {
        y[s, t, j] ~ dbern(z[s, t] * p_s[s])
      }
    }
  }
})

# build and compile -------------------------------------------------------

model <- nimbleModel(
  code = occupancy_code,
  constants = sim$constants,
  data = sim$data,
  inits = sim$inits(),
  buildDerivs = TRUE
)
compiled_model <- compileNimble(model)

mcmc_conf <- configureMCMC(model, monitors = c(
  "beta0", "zeta", "phi",
  "alpha0", 
  # "alpha1", 
  "psi1",
  "x_prime", "y_prime"
))

# mcmc_conf$removeSampler(c("beta0", "zeta"))
# mcmc_conf$addSampler(target = c("beta0", "zeta"), type = "barker")

mcmc <- buildMCMC(mcmc_conf)
compiled_mcmc <- compileNimble(mcmc, project = model)

# run MCMC ----------------------------------------------------------------

samples <- runMCMC(
  compiled_mcmc,
  niter = 40000,
  nburnin = 10000,
  thin = 10,
  nchains = 4,
  inits = sim$inits,
  samplesAsCodaMCMC = TRUE
)

# results -----------------------------------------------------------------

sim$params

MCMCtrace(samples, pdf = FALSE)
MCMCsummary(samples)
bayesplot::mcmc_pairs(as.matrix(samples), pars = c("beta0", "zeta"))

# gamma preds -------------------------------------------------------------

summary_stats <- summary(samples)$statistics
beta0_use <- summary_stats["beta0", "Mean"]
zeta_use  <- summary_stats["zeta", "Mean"]
x_prime_use <- summary_stats["x_prime", "Mean"]
y_prime_use <- summary_stats["y_prime", "Mean"]

grid_res <- 100
grid <- expand.grid(
  x = seq(min(sim$grid$x), max(sim$grid$x), length.out = grid_res),
  y = seq(min(sim$grid$y), max(sim$grid$y), length.out = grid_res),
  t = sim$time_points
)

grid$distance <- -sqrt((grid$x - x_prime_use)^2 + (grid$y - y_prime_use)^2)
grid$rho <- grid$t + (1 / zeta_use) * grid$distance
grid$gamma <- plogis(beta0_use + grid$rho)

ggplot(grid, aes(x = x, y = y, fill = gamma)) +
  geom_tile() +
  scale_fill_viridis_c(name = expression(gamma), option = "C") +
  geom_point(aes(x = x_prime_use, y = y_prime_use), color = "red", shape = 4, size = 4, stroke = 1.5) +
  facet_wrap(~t) +
  theme_minimal()

# Visual sanity check
sim$plots$occupancy
