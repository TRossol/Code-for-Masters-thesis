library(ggplot2)
library(extraDistr)

# generate model parameters in a vector rho
generate_rho <- function(n, lower = -3, upper = 3, exclude_lower = -0.5, exclude_upper = 0.5) {
  rho <- numeric(n)
  for (i in 1:n) {
    repeat {
      value <- runif(1, lower, upper)
      if (value < exclude_lower || value > exclude_upper) {
        rho[i] <- value
        break
      }
    }
  }
  return(rho)
}


asymptotic_variance <- function(D, Z, Y) {
  meanD2 <- mean(D^2)
  meanD4 <- mean(D^4)
  meanDY <- mean(D * Y)
  meanDZ <- mean(D * Z)
  meanYZ <- mean(Y * Z)
  meanD2Z <- mean(D^2 * Z)
  meanDZ2 <- mean(D * Z^2)
  meanD2Y2 <- mean(D^2 * Y^2)
  meanD2Z2 <- mean(D^2 * Z^2)
  meanY2Z2 <- mean(Y^2 * Z^2)
  meanD2Z3 <- mean(D^2 * Z^3)
  meanD3Z <- mean(D^3 * Z)
  meanD3Z2 <- mean(D^3 * Z^2)
  meanD3Z3 <- mean(D^3 * Z^3)
  meanD4Z <- mean(D^4 * Z)
  meanD4Z2 <- mean(D^4 * Z^2)
  meanD2Z4 <- mean(D^2 * Z^4)
  meanD3Y <- mean(D^3 * Y)
  meanD2YZ <- mean(D^2 * Y * Z)
  meanDY2Z <- mean(D * Y^2 * Z)
  meanDYZ2 <- mean(D * Y * Z^2)
  meanD2YZ2 <- mean(D^2 * Y * Z^2)
  meanD3YZ <- mean(D^3 * Y * Z)
  meanDYZ3 <- mean(D * Y * Z^3)
  
  # just a savety check, if the denominator is to small
  # this is for debugging to check if it is happening systematically
  if (abs(meanD2 * meanDZ2 - meanDZ * meanD2Z) < 1e-6) {
    stop("Condition met: meanD2 * meanDZ2 - meanDZ * meanD2Z is approximately 0 within a tolerance of 1e-6")
  }
  
  av <- (meanY2Z2*meanD2^2*meanD2Z^2*meanDZ2^2 
         - 2*meanDYZ3*meanD2^2*meanD2Z^2*meanDZ2*meanYZ 
         + meanD2Z4*meanD2^2*meanD2Z^2*meanYZ^2 
         - 2*meanDY2Z*meanD2^2*meanD2Z*meanDZ2^3 
         + 4*meanD2YZ2*meanD2^2*meanD2Z*meanDZ2^2*meanYZ 
         - 2*meanD3Z3*meanD2^2*meanD2Z*meanDZ2*meanYZ^2 
         + meanD2Y2*meanD2^2*meanDZ2^4 
         - 2*meanD3YZ*meanD2^2*meanDZ2^3*meanYZ 
         + meanD4Z2*meanD2^2*meanDZ2^2*meanYZ^2 
         + 2*meanDYZ3*meanD2*meanDY*meanDZ*meanD2Z^2*meanDZ2 
         - 2*meanD2Z4*meanD2*meanDY*meanDZ*meanD2Z^2*meanYZ 
         - 4*meanD2YZ2*meanD2*meanDY*meanDZ*meanD2Z*meanDZ2^2 
         + 4*meanD3Z3*meanD2*meanDY*meanDZ*meanD2Z*meanDZ2*meanYZ 
         + 2*meanD3YZ*meanD2*meanDY*meanDZ*meanDZ2^3 
         - 2*meanD4Z2*meanD2*meanDY*meanDZ*meanDZ2^2*meanYZ 
         - 2*meanDYZ2*meanD2*meanDY*meanD2Z^2*meanDZ2^2 
         + 2*meanD2Z3*meanD2*meanDY*meanD2Z^2*meanDZ2*meanYZ 
         + 4*meanD2YZ*meanD2*meanDY*meanD2Z*meanDZ2^3 
         - 4*meanD3Z2*meanD2*meanDY*meanD2Z*meanDZ2^2*meanYZ 
         - 2*meanD3Y*meanD2*meanDY*meanDZ2^4 
         + 2*meanD4Z*meanD2*meanDY*meanDZ2^3*meanYZ 
         - 2*meanY2Z2*meanD2*meanDZ*meanD2Z^3*meanDZ2 
         + 2*meanDYZ3*meanD2*meanDZ*meanD2Z^3*meanYZ 
         + 4*meanDY2Z*meanD2*meanDZ*meanD2Z^2*meanDZ2^2 
         - 4*meanD2YZ2*meanD2*meanDZ*meanD2Z^2*meanDZ2*meanYZ 
         - 2*meanD2Y2*meanD2*meanDZ*meanD2Z*meanDZ2^3 
         + 2*meanD3YZ*meanD2*meanDZ*meanD2Z*meanDZ2^2*meanYZ 
         + 2*meanDYZ2*meanD2*meanD2Z^3*meanDZ2*meanYZ 
         - 2*meanD2Z3*meanD2*meanD2Z^3*meanYZ^2 
         - 4*meanD2YZ*meanD2*meanD2Z^2*meanDZ2^2*meanYZ 
         + 4*meanD3Z2*meanD2*meanD2Z^2*meanDZ2*meanYZ^2 
         + 2*meanD3Y*meanD2*meanD2Z*meanDZ2^3*meanYZ 
         - 2*meanD4Z*meanD2*meanD2Z*meanDZ2^2*meanYZ^2 
         + meanD2Z4*meanDY^2*meanDZ^2*meanD2Z^2 
         - 2*meanD3Z3*meanDY^2*meanDZ^2*meanD2Z*meanDZ2 
         + meanD4Z2*meanDY^2*meanDZ^2*meanDZ2^2 
         - 2*meanD2Z3*meanDY^2*meanDZ*meanD2Z^2*meanDZ2 
         + 4*meanD3Z2*meanDY^2*meanDZ*meanD2Z*meanDZ2^2 
         - 2*meanD4Z*meanDY^2*meanDZ*meanDZ2^3 
         + meanD2Z2*meanDY^2*meanD2Z^2*meanDZ2^2 
         - 2*meanD3Z*meanDY^2*meanD2Z*meanDZ2^3 
         + meanD4*meanDY^2*meanDZ2^4 
         - 2*meanDYZ3*meanDY*meanDZ^2*meanD2Z^3 
         + 4*meanD2YZ2*meanDY*meanDZ^2*meanD2Z^2*meanDZ2 
         - 2*meanD3YZ*meanDY*meanDZ^2*meanD2Z*meanDZ2^2 
         + 2*meanDYZ2*meanDY*meanDZ*meanD2Z^3*meanDZ2 
         + 2*meanD2Z3*meanDY*meanDZ*meanD2Z^3*meanYZ 
         - 4*meanD2YZ*meanDY*meanDZ*meanD2Z^2*meanDZ2^2 
         - 4*meanD3Z2*meanDY*meanDZ*meanD2Z^2*meanDZ2*meanYZ 
         + 2*meanD3Y*meanDY*meanDZ*meanD2Z*meanDZ2^3 
         + 2*meanD4Z*meanDY*meanDZ*meanD2Z*meanDZ2^2*meanYZ 
         - 2*meanD2Z2*meanDY*meanD2Z^3*meanDZ2*meanYZ 
         + 4*meanD3Z*meanDY*meanD2Z^2*meanDZ2^2*meanYZ 
         - 2*meanD4*meanDY*meanD2Z*meanDZ2^3*meanYZ 
         + meanY2Z2*meanDZ^2*meanD2Z^4 
         - 2*meanDY2Z*meanDZ^2*meanD2Z^3*meanDZ2 
         + meanD2Y2*meanDZ^2*meanD2Z^2*meanDZ2^2 
         - 2*meanDYZ2*meanDZ*meanD2Z^4*meanYZ 
         + 4*meanD2YZ*meanDZ*meanD2Z^3*meanDZ2*meanYZ 
         - 2*meanD3Y*meanDZ*meanD2Z^2*meanDZ2^2*meanYZ 
         + meanD2Z2*meanD2Z^4*meanYZ^2 
         - 2*meanD3Z*meanD2Z^3*meanDZ2*meanYZ^2 
         + meanD4*meanD2Z^2*meanDZ2^2*meanYZ^2)/((meanD2*meanDZ2 - meanDZ*meanD2Z)^4) 
  return(av)
}


# generate random noise
generate_random_noise <- function(distr_name, params, size) {
  distr_name <- tolower(distr_name) # Convert input to lowercase for flexibility
  
  # Extract dimensions
  n_rows <- size[1]
  n_cols <- size[2]
  
  # Generate data as a data frame
  df <- as.data.frame(
    replicate(n_cols, {
      if (distr_name == "poisson") {
        if (length(params) != 1) stop("Poisson distribution requires one parameter: lambda")
        rpois(n_rows, lambda = params[1])
      } else if (distr_name %in% c("exponential", "exp")) {
        if (length(params) != 1) stop("Exponential distribution requires one parameters: rate")
        rexp(n_rows, rate = params[1])
      } else if (distr_name %in% c("gamma")) {
        if (length(params) != 2) stop("Gamma distribution requires two parameters: rate, shape")
        rgamma(n_rows, shape = params[1], scale = params[2])
      } else if (distr_name %in% c("weibull")) {
        if (length(params) != 2) stop("Weibull distribution requires two parameters: shape, scale")
        rweibull(n_rows, params[1], params[2])
      } else if (distr_name %in% c("beta")) {
        if (length(params) != 2) stop("Beta distribution requires two parameters: shape1, shape2")
        rbeta(n_rows, params[1], params[2])
      } else if (distr_name %in% c("chi-squared")) {
        if (length(params) != 1) stop("Chi-squared distribution requires one parameters: df")
        rchisq(n_rows, df = params[1])
      } else if (distr_name %in% c("inv. gamma")) {
        if (length(params) != 2) stop("Inverse Gamma distribution requires two parameters: alpha, beta")
        rinvgamma(n_rows, params[1], params[2])
      } else {
        stop("Unsupported distribution type.")
      }
    })
  )
  
  # Rename columns to V1, V2, ..., Vn
  colnames(df) <- paste0("V", seq_len(n_cols))
  
  return(df)
}

# generate model variables and calculate estimators
generate_estimations_for_model_M <- function(distr_name, params_epsilons, rho, n) {
  df_eps <- generate_random_noise(distr_name, params_epsilons, c(n, 4))
  epsilon_u <- df_eps$V1
  epsilon_z <- df_eps$V2
  epsilon_d <- df_eps$V3
  epsilon_y <- df_eps$V4
  
  U <- epsilon_u
  Z <- rho[2] * U + epsilon_z
  D <- rho[3] * U + epsilon_d
  Y <- rho[1] * D + rho[4] * U + epsilon_y
  
  U <- U - mean(U)
  Z <- Z - mean(Z)
  D <- D - mean(D)
  Y <- Y - mean(Y)
  
  x1 <- mean(D^2)
  x2 <- mean(D*Z)
  x3 <- mean(D*Y)
  x4 <- mean(Y*Z)
  x5 <- mean(D * Z^2)
  x6 <- mean(D^2 * Z)
  
  # emprirical saftey checks
  if ((abs(x5) < 1e-6) || (abs(x1 - (x6 / x5) * x2)) < 1e-6) {
    stop("Potential division by zero or very small values in the denominator.")
  }
  
  beta_hat <- (x3 - (x6 / x5) * x4) / (x1 - (x6 / x5) * x2)
  beta <- rho[1]
  av <- asymptotic_variance(D, Z, Y)
  
  # safety check, that av > 0
  if (av <= 0) {
    stop("Asymptotic variance lesser or equal zero, this makes no sense.")
  }
  
  return(list(
    beta_hat = beta_hat, beta = beta, asymptotic_var = av,
    model_realizations = data.frame(U = U, Z = Z, D = D, Y = Y)
  ))
}

generate_plots <- function(
    m,
    list_of_n,
    distr_name,
    params_epsilons,
    rho,
    plot_type,
    confidence_level = 0.90) {
  
  # create empty list to store the final results for every n in list_of_n
  beta_hat_for_n <- numeric()
  lower_bounds_for_n <- numeric()
  upper_bounds_for_n <- numeric()
  is_in_interval_for_n <- numeric()
  
  quantile <- qnorm((1 + confidence_level) / 2)
  for (n in list_of_n) {
    
    # initialize empty vector to store the results for every m in 1:m
    beta_hat_for_m <- numeric()
    lower_bounds_for_m <- numeric()
    upper_bounds_for_m <- numeric()
    is_in_interval_for_m <- numeric()
    av_for_m <- numeric()
    
    for (i in 1:m) {
      estimations <- generate_estimations_for_model_M(
        distr_name, params_epsilons, rho, n
      )
      
      # add beta_hat to vector
      beta_hat_for_m <- c(beta_hat_for_m, estimations$beta_hat)
      
      # get quantile value
      av <- estimations$asymptotic_var
      av_for_m <- c(av_for_m, av)
      
      # calculate interval bounds
      lower_bounds_for_m <- c(lower_bounds_for_m, estimations$beta_hat - quantile * sqrt(av / n))
      upper_bounds_for_m <- c(upper_bounds_for_m, estimations$beta_hat + quantile * sqrt(av / n))
      
      # determine if beta is in interval
      is_within_interval <- (rho[1] >= lower_bounds_for_m[i] && rho[1] <= upper_bounds_for_m[i])
      is_in_interval_for_m <- c(is_in_interval_for_m, is_within_interval)
    }
    
    # calculate the median of beta_hat
    beta_hat_for_n <- c(beta_hat_for_n, median(beta_hat_for_m))
    
    # calculate the median of asymptotic variance
    median_av <- median(av_for_m)
    
    # use these medians to construct confidence interval 
    lower_bounds_for_n <- c(lower_bounds_for_n, median(beta_hat_for_m) - quantile * sqrt(median_av / n))
    upper_bounds_for_n <- c(upper_bounds_for_n, median(beta_hat_for_m) + quantile * sqrt(median_av / n))
    
    # calculate the % of 1s in the vector
    is_in_interval_for_n <- c(is_in_interval_for_n, sum(is_in_interval_for_m) / m)
  }
  
  # collect results
  results_df <- data.frame(
    "n" = list_of_n,
    "beta_hat" = beta_hat_for_n,
    "lower_bounds" = lower_bounds_for_n,
    "upper_bounds" = upper_bounds_for_n,
    "coverage" = is_in_interval_for_n
  )
  
  # create plots
  if (plot_type == "interval") {
    p <- create_interval_plot(results_df, distr_name, params_epsilons, rho)
  } else if (plot_type == "coverage") {
    p <- create_coverage_plot(results_df, distr_name, params_epsilons, rho, confidence_level)
  } else {
    stop("Unsupported plot type.")
  }
  
  return(p)
}



create_interval_plot <- function(df_plot, distr_name, params_epsilons, rho) {
  
  # overwrite the first string in the distr_name with an uppercase letter
  distr_name <- paste(toupper(substr(distr_name, 1, 1)), substr(distr_name, 2, nchar(distr_name)), sep = "")
  
  # write the string for the title
  title_str <- distr_name
  title_str <- paste0(title_str, "(", paste(params_epsilons, collapse = ", "), ")")
  title_str <- paste0(title_str, ", model parameters ", "(", paste(rho, collapse = ", "), ")")
  
  # create plot
  plot <- ggplot(df_plot, aes(x = n)) +
    geom_line(aes(y = lower_bounds, color = "bounds"), linewidth = 0.7) +
    geom_line(aes(y = upper_bounds, color = "bounds"), linewidth = 0.7) +
    geom_point(aes(y = beta_hat, color = "estimators"), size = 0.5) +
    geom_hline(aes(yintercept = rho[1], color = "real beta"), linewidth = 0.5, linetype = "dashed") +
    scale_color_manual(values = c("bounds" = "#010a6e", "estimators" = "#8d203bfb", "real beta" = "#095a2f")) +
    labs(
      title = title_str,
      x = "Sample size (n)",
      y = "Confidence interval",
      color = "Legend"
    ) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.25),
      panel.grid.minor = element_line(color = "gray90", linewidth = 0.25),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 14) 
    )
  return(plot)
}


create_coverage_plot <- function(df_coverage_plot, distr_name, params_epsilons, rho, confidence_level) {
  
  # overwrite the first string in the distr_name with an uppercase letter
  distr_name <- paste(toupper(substr(distr_name, 1, 1)), substr(distr_name, 2, nchar(distr_name)), sep = "")
  
  # round the rho values to 2 decimal places, for display
  rho <- round(rho, 2)
  
  # write the string for the title
  title_str <- distr_name
  title_str <- paste0(title_str, "(", paste(params_epsilons, collapse = ", "), ")")
  title_str <- paste0(title_str, ", model parameters ", "(", paste(rho, collapse = ", "), ")")
  
  # create plot
  plot <- ggplot(df_coverage_plot) +
    geom_line(aes(x = n, y = coverage, color = "coverage"), linewidth = 0.5) +
    geom_point(aes(x = n, y = coverage, color = "coverage"), size = 1) +
    geom_hline(aes(yintercept = confidence_level, color = "confidence level"), linetype = "dashed") +
    scale_color_manual(values = c("coverage" = "#095a2f", "confidence level" = "#8d203bfb")) +
    labs(
      title = title_str,
      x = "Sample size (n)",
      y = "Coverage",
      color = "Legend"
    ) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.25),
      panel.grid.minor = element_line(color = "gray90", linewidth = 0.25),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 14) 
    )
  
  return(plot)
}
