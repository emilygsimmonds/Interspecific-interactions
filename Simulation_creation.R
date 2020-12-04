# This script runs simulations to create time series data of species i and j

###############################################################################

# If you open in RStudio go to Edit > Folding > Collapse all

###############################################################################

# Set up

#### Load required packages ####

require(tidyverse)
# you will also need packages MASS and dplyr installed

#### Source user defined functions ####

# function to iterate the simulation
source("./Simulation_function.R")
# uses the Gompertz_function.R

###############################################################################

#### BASELINE SCENARIO 0.5 strength ####

# MUTUALISM - negative to begin

r <- c(1.25, 1.25) # intrinsic growth rates
K <- log(125) # Carrying capacities
a_joni <- -0.5 # effect of j on i
a_ionj <- -0.5 # effect of i on j
c_i <- 1+(-r[1]/K)
c_j <- 1+(-r[2]/K) # calculate intra-specific competition
alpha_ionj <- (-r[1]*a_ionj)/K # calculate inter-specific effects
alpha_joni <- (-r[2]*a_joni)/K

# now combine c and alphas into a single matrix
alphas_m <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                 ncol = 2,
                 byrow = TRUE)

# Create other inputs for Simulation_func

n <- 50
starts <- c(100,100)
tau <- 0.2 
rho <- 0.007

simulations_output_m <- purrr::rerun(100, 
                                  Simulation_func(n = n,
                                  starts = log(starts),
                                  r = r,
                                  alphas = alphas_m,
                                  tau=tau, 
                                  rho = rho,
                                  burnin = 50)) 
# PREDATOR-PREY

r <- c(1.25, 1.25) # intrinsic growth rates
K <- log(125) # Carrying capacities
a_joni <- 0.5 # effect of j on i
a_ionj <- -0.5 # effect of i on j
c_i <- 1+(-r[1]/K)
c_j <- 1+(-r[2]/K) # calculate intra-specific competition
alpha_ionj <- (-r[1]*a_ionj)/K # calculate inter-specific effects
alpha_joni <- (-r[2]*a_joni)/K

# now combine c and alphas into a single matrix
alphas_p <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                 ncol = 2,
                 byrow = TRUE)

# Create other inputs for Simulation_func

n <- 50
starts <- c(100,100)
tau <- 0.2 
rho <- 0.007

simulations_output_p <- purrr::rerun(100, 
                                     Simulation_func(n = n,
                                                     starts = log(starts),
                                                     r = r,
                                                     alphas = alphas_p,
                                                     tau=tau, 
                                                     rho = rho,
                                                     burnin = 50,
                                                     maxiter = 100)) 

# COMPETITION

r <- c(1.25, 1.25) # intrinsic growth rates
K <- log(125) # Carrying capacities
a_joni <- 0.5 # effect of j on i
a_ionj <- 0.5 # effect of i on j
c_i <- 1+(-r[1]/K)
c_j <- 1+(-r[2]/K) # calculate intra-specific competition
alpha_ionj <- (-r[1]*a_ionj)/K # calculate inter-specific effects
alpha_joni <- (-r[2]*a_joni)/K

# now combine c and alphas into a single matrix
alphas_c <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                 ncol = 2,
                 byrow = TRUE)

# Create other inputs for Simulation_func

n <- 50
starts <- c(100,100)
tau <- 0.2 
rho <- 0.007

simulations_output_c <- purrr::rerun(100, 
                                     Simulation_func(n = n,
                                                     starts = log(starts),
                                                     r = r,
                                                     alphas = alphas_c,
                                                     tau=tau, 
                                                     rho = rho,
                                                     burnin = 50,
                                                     maxiter = 50)) 


###############################################################################

#### Combine and save ####

simulations_all <- c(simulations_output_c, 
                     simulations_output_m, 
                     simulations_output_p)

save(simulations_all, file="simulated_TS_2020.RData")
