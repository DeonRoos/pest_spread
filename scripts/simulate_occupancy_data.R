simulate_occupancy_data <- function(
    grid_cell_size = 10,
    spatial_extent = list(x = c(0, 100), y = c(0, 100)),
    total_days = 30,
    sampling_interval = 2,
    J = 3,
    beta0 = -2,
    beta1 = 1,
    zeta = 5,
    phi = 0.8,
    alpha0 = 4,
    alpha1 = -0.2,
    psi1 = 0.1,
    spread_center = NULL,
    detect_spatial = TRUE,
    sample_extent = NULL,
    inhomogeneous = FALSE,
    n_sites = NULL,
    seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)
  
  # site coordinates --------------------------------------------------------
  if (!inhomogeneous) {
    x_coords <- seq(spatial_extent$x[1], spatial_extent$x[2], by = grid_cell_size)
    y_coords <- seq(spatial_extent$y[1], spatial_extent$y[2], by = grid_cell_size)
    grid <- expand.grid(x = x_coords, y = y_coords)
  } else {
    if (is.null(n_sites)) stop("When inhomogeneous = TRUE, n_sites must be specified.")
    grid <- data.frame(
      x = runif(n_sites, spatial_extent$x[1], spatial_extent$x[2]),
      y = runif(n_sites, spatial_extent$y[1], spatial_extent$y[2])
    )
  }
  S <- nrow(grid)
  
  # spread center -----------------------------------------------------------
  if (is.null(spread_center)) {
    x_prime <- mean(spatial_extent$x)
    y_prime <- mean(spatial_extent$y)
  } else {
    x_prime <- spread_center[1]
    y_prime <- spread_center[2]
  }
  
  # time setup --------------------------------------------------------------
  T_full <- total_days
  time_points <- seq(1, T_full, by = sampling_interval)
  T_sampled <- length(time_points)
  
  # occupancy/detection arrays ----------------------------------------------
  z <- matrix(0, nrow = S, ncol = T_full)
  y <- array(0, dim = c(S, T_sampled, J))
  
  # distances ---------------------------------------------------------------
  dist_raw <- -sqrt((grid$x - x_prime)^2 + (grid$y - y_prime)^2)
  dist_to_center <- -dist_raw
  
  # detection probability ---------------------------------------------------
  if (detect_spatial) {
    p_s <- plogis(alpha0 + alpha1 * dist_to_center)
  } else {
    p_s <- rep(plogis(alpha0), S)
  }
  
  # sampling mask -----------------------------------------------------------
  if (!is.null(sample_extent)) {
    sampled_sites <- with(grid,
                          x >= sample_extent[1] & x <= sample_extent[2] &
                            y >= sample_extent[3] & y <= sample_extent[4])
  } else {
    sampled_sites <- rep(TRUE, S)
  }
  
  # initial occupancy -------------------------------------------------------
  initial_site <- which.min(dist_to_center)
  z[, 1] <- 0
  z[initial_site, 1] <- 1
  
  # occupancy dynamics ------------------------------------------------------
  for (t in 2:T_full) {
    rho <- t + (1 / zeta) * dist_raw
    gamma <- plogis(beta0 + beta1 * rho) # default leaves beta1 = 1, in which case can be ignored in model to save parameter
    for (s in 1:S) {
      z[s, t] <- if (z[s, t - 1] == 1) rbinom(1, 1, phi) else rbinom(1, 1, gamma[s])
    }
  }
  
  # detection process -------------------------------------------------------
  for (t_idx in seq_along(time_points)) {
    t <- time_points[t_idx]
    for (s in 1:S) {
      if (sampled_sites[s]) {
        for (j in 1:J) {
          y[s, t_idx, j] <- rbinom(1, 1, z[s, t] * p_s[s])
        }
      } else {
        y[s, t_idx, ] <- NA
      }
    }
  }
  
  # plots -------------------------------------------------------------------
  plot_data <- do.call(rbind, lapply(1:T_full, function(t) {
    data.frame(
      site = 1:S,
      x = grid$x,
      y = grid$y,
      occupied = factor(z[, t], levels = c(0, 1), labels = c("Unoccupied", "Occupied")),
      t = t
    )
  }))
  
  spread_center_df <- data.frame(
    x = x_prime,
    y = y_prime,
    t = 1:T_full
  )
  
  p_occ <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_point(aes(color = occupied), size = 2, alpha = 0.8) +
    scale_color_manual(values = c("lightgray", "blue")) +
    geom_point(data = spread_center_df, aes(x = x, y = y), shape = 4, size = 4, color = "red", stroke = 1.5) +
    facet_wrap(~t, ncol = 7) +
    theme_minimal() +
    labs(color = "Occupancy") +
    theme(strip.text = element_text(size = 12))
  
  detection_summary <- apply(y, c(1, 2), max, na.rm = TRUE)
  
  detection_data <- do.call(rbind, lapply(1:T_sampled, function(t_idx) {
    data.frame(
      site = 1:S,
      x = grid$x,
      y = grid$y,
      detected = factor(detection_summary[, t_idx], levels = c(0, 1), labels = c("Not Detected", "Detected")),
      t = time_points[t_idx]
    )
  }))
  
  p_det <- ggplot(detection_data, aes(x = x, y = y)) +
    geom_point(aes(color = detected), size = 2, alpha = 0.8) +
    scale_color_manual(values = c("lightgray", "darkgreen")) +
    geom_point(data = spread_center_df, aes(x = x, y = y), shape = 4, size = 4, color = "red", stroke = 1.5) +
    facet_wrap(~t, ncol = 7) +
    theme_minimal() +
    labs(color = "Detection") +
    theme(strip.text = element_text(size = 12))
  
  # nimble-ready outputs ----------------------------------------------------
  constants <- list(
    S = S,
    T = T_sampled,
    J = J,
    x_coord = grid$x,
    y_coord = grid$y,
    time = time_points
  )
  
  data <- list(y = y)
  
  inits <- function() {
    list(
      z = matrix(1, nrow = S, ncol = T_full),
      beta0 = rnorm(1, 0, 2),
      beta1 = rnorm(1, 0, 3),
      zeta = rgamma(1, 3, 1.5),
      phi = rbeta(1, 8, 2),
      alpha0 = rnorm(1, 0, 3),
      alpha1 = rnorm(1, 0, 1),
      psi1 = rbeta(1, 1, 1),
      x_prime = runif(1, spatial_extent$x[1], spatial_extent$x[2]),
      y_prime = runif(1, spatial_extent$y[1], spatial_extent$y[2])
    )
  }
  
  return(list(
    grid = grid,
    z = z,
    y = y,
    dist_raw = dist_raw,
    dist_to_center = dist_to_center,
    time_points = time_points,
    sampled_sites = sampled_sites,
    plots = list(occupancy = p_occ, detection = p_det),
    constants = constants,
    data = data,
    inits = inits,
    params = list(
      beta0 = beta0,
      zeta = zeta,
      phi = phi,
      alpha0 = alpha0,
      alpha1 = alpha1,
      psi1 = psi1,
      spread_center = c(x_prime, y_prime),
      spatial_extent = spatial_extent,
      sampling_interval = sampling_interval,
      detect_spatial = detect_spatial,
      sample_extent = sample_extent,
      inhomogeneous = inhomogeneous,
      n_sites = n_sites
    )
  ))
}
