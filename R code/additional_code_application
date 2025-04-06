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
##    Figure A.2    ##
######################
# with decrease ∝ 1/sqrt(n)

plot_type <- "interval"
m <- 100 
n_list <- seq(100, 7500, by = 100) 

# generate plot
plot5 <- generate_plots(
  m = m,
  list_of_n = n_list,
  distr_name = dist_name,
  params_epsilons = parameters,
  rho = rho,
  plot_type = plot_type,
  confidence_level = confidence_level
)

# calculate the curve data for the dashed line
curve_data <- data.frame(
  n = n_list,
  y1 = 8 / sqrt(n_list) + 2.49,
  y2 = -8 / sqrt(n_list) + 2.49
)
# add plot to plot5-plot
plot5 <- plot5 +
  geom_line(data = curve_data, aes(x = n, y = y1), color = "red", linetype = "dashed") +
  geom_line(data = curve_data, aes(x = n, y = y2), color = "red", linetype = "dashed")

####################
