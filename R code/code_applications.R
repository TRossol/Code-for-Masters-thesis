# import functions from Code_Formulas
source("code_functions.R")
set.seed(42) # set random seed
rho <- generate_rho(4)
rho <- round(rho, 2)
print(rho)

confidence_level <- 0.90  # fix a confidence level
dist_name <- "Gamma"      # fix a distribution
parameters <- c(2,1)      # fix parameters for the distribution


######################
##    Figure 4.1    ##
######################

plot_type <- "interval"
m <- 100
n_list <- seq(100, 7500, by = 100)

# generate plot
plot1 <- generate_plots(
  m = m,
  list_of_n = n_list,
  distr_name = dist_name,
  params_epsilons = parameters,
  rho = rho,
  plot_type = plot_type,
  confidence_level = confidence_level
)
####################

######################
##    Figure 4.2    ##
######################

plot_type <- "interval"
m <- 100
n_list <- seq(10, 500, by = 10)

# generate plot
plot2 <- generate_plots(
  m = m,
  list_of_n = n_list,
  distr_name = dist_name,
  params_epsilons = parameters,
  rho = rho,
  plot_type = plot_type,
  confidence_level = confidence_level
)
####################

######################
##    Figure 4.3    ##
######################

plot_type <- "interval"
m <- 100
n_list <- seq(1000, 10000, by = 100)

# generate plot
plot3 <- generate_plots(
  m = m,
  list_of_n = n_list,
  distr_name = dist_name,
  params_epsilons = parameters,
  rho = rho,
  plot_type = plot_type,
  confidence_level = confidence_level
)
####################

######################
##    Figure 4.4    ##
######################

plot_type <- "coverage"
m <- 1000
n_list <- seq(10, 500, by = 1)

# generate plot
plot4 <- generate_plots(
  m = m,
  list_of_n = n_list,
  distr_name = dist_name,
  params_epsilons = parameters,
  rho = rho,
  plot_type = plot_type,
  confidence_level = confidence_level
)
####################




