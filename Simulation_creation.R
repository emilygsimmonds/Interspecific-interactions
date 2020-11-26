# This script runs simulations to create time series data of species i and j

###############################################################################

# If you open in RStudio go to Edit > Folding > Collapse all

###############################################################################

# Set up

#### Load required packages ####

require(tidyverse) # Sorry Bert!
# you will also need package MASS installed

#### Source user defined functions ####

# function to iterate the simulation
source("./Simulation_function.R")
# uses the Gompertz_function.R

###############################################################################

#### Define parameters ####

# MUTUALISM

r <- c(1.25, 1.25) # intrinsic growth rates
K <- log(125) # Carrying capacities
a_joni <- 0.7 # effect of j on i
a_ionj <- 0.7 # effect of i on j
c_i <- 1+(-r[1]/K)
c_j <- 1+(-r[2]/K) # calculate intra-specific competition
alpha_ionj <- (-r[1]*a_ionj)/K # calculate inter-specific effects
alpha_joni <- (-r[2]*a_joni)/K

# now combine c and alphas into a single matrix
alphas <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                 ncol = 2,
                 byrow = TRUE)

# Create other inputs for Simulation_func

n <- 50
starts <- c(100,100)
tau <- 0.2 
rho <- 0.007

simulations_output_m <- purrr::rerun(10, 
                                  Simulation_func(n = n,
                                  starts = log(starts),
                                  r = r,
                                  alphas = alphas,
                                  tau=tau, 
                                  rho = rho,
                                  burnin = 50)) 
# PREDATOR-PREY

r_i <- r_j <- 1.25 # intrinsic growth rate
K <- log(120) # Carrying capacity
a_i <- 0.6 # effect of j on i
a_j <- -0.6 # effect of i on j
c_i <- c_j <- 1+(-r_i/K) # calculate intra-specific competition
alpha_ij <- (-r_i*a_i)/K # calculate inter-specific effects
alpha_ji <- (-r_j*a_j)/K

# Store these parameters in a dataframe for input to Simulation_func

parameters_pp <- data.frame(n = 50,
                            start_i = 100,
                            start_j = 100,
                            r_i = r_i,
                            r_j = r_j,
                            c_i = c_i,
                            c_j = c_j,
                            alpha_ij = rep(alpha_ij, each = 100),
                            alpha_ji = rep(alpha_ji, each = 100),
                            tau = 0.2, Rho = 0.007
) %>% split(seq(100))

# COMPETITION

r_i <- r_j <- 1.25 # intrinsic growth rate
K <- log(120) # Carrying capacity
a_i <- -0.5 # effect of j on i
a_j <- -0.5 # effect of i on j
c_i <- c_j <- 1+(-r_i/K) # calculate intra-specific competition
alpha_ij <- (-ri*a_i)/K # calculate inter-specific effects
alpha_ji <- (-rj*a_j)/K

# Store these parameters in a dataframe for input to Simulation_func

parameters_c <- data.frame(n = 50,
                           start_i = 100,
                           start_j = 100,
                           r_i = r_i,
                           r_j = r_j,
                           c_i = c_i,
                           c_j = c_j,
                           alpha_ij = rep(alpha_ij, each = 100),
                           alpha_ji = rep(alpha_ji, each = 100),
                           tau = 0.2, Rho = 0.007
) %>% split(seq(100))

###############################################################################

#### Simulate ####

# MUTUALISM

simulations_output_m <- map(parameters_m, Simulation_func) # run simulations

# PREDATOR-PREY

simulations_output_pp <- map(parameters_pp, Simulation_func) # run simulations

# COMPETITION

simulations_output_c <- map(parameters_c, Simulation_func) # run simulations

###############################################################################

#### Combine and save ####

simulations_all <- c(simulations_output_c, 
                     simulations_output_m, 
                     simulations_output_pp)

save(simulations_all, file="simulations_output_2020.RData")
