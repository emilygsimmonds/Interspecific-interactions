# This script runs simulations to choose the parameters for each scenario

###############################################################################

# If you open in RStudio go to Edit > Folding > Collapse all

###############################################################################

# Set up

#### Load required packages ####

require(tidyverse)
require(patchwork)
# you will also need packages MASS and dplyr installed

#### Source user defined functions ####

# function to iterate the simulation
source("./Simulation_function.R")
# uses the Gompertz_function.R

###############################################################################

#### SINGLE SPECIES ####

ss <- function(r, K, tau, N){
  
  c <- 1+(-r/K)
  e <- rnorm(1000, 0, tau)
  
  for(i in 2:length(e)){
  N[i] <- r + (c*N[i-1]) + e[i]
  }
  
  return(data.frame(N = N,
                    time = seq(1, length(e), 1)))
}

# Aim is for stability over 1000 years

r <- 1 # intrinsic growth rate
K <- log(100) # Carrying capacity
tau <- 0.05
N <- rep(log(100), 1000)

single_species_results <- purrr::rerun(100,
             ss(r, K, tau, N))

i <- as.list(1:100)

single_species_results_df <- purrr::map2_dfr(.x = single_species_results,
                                 .y = i, ~{
                                   .x$Scenario <- .y 
                                   return(.x)
                                 })

ggplot(data = filter(single_species_results_df),
       aes(x = time,
           y = exp(N), 
           colour = Scenario))+
  geom_line()


#### COMPETITION ####

# Aim is for stability over 1000 years

r <- c(1, 1) # intrinsic growth rates
K <- c(log(100), log(100)) # Carrying capacities
a_joni <- 0.5 # effect of j on i
a_ionj <- 0.5 # effect of i on j
c_i <- 1+(-r[1]/K[1])
c_j <- 1+(-r[2]/K[2]) # calculate intra-specific competition
tau <- 0.05
corr <- 0.7
rho <- corr*tau # correlation = rho/tau
alpha_ionj <- (-r[1]*a_ionj)/K[1] # calculate inter-specific effects
alpha_joni <- (-r[2]*a_joni)/K[2]

alphas <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                   ncol = 2,
                   byrow = TRUE)

starts <- c(100,100)
n = 1000

cc_results <- purrr::rerun(100, 
                          Simulation_func(n = n,
                                          starts = log(starts),
                                          r = r,
                                          alphas = alphas,
                                          tau = tau, 
                                          rho = rho,
                                          burnin = 0)) 

i <- as.list(1:100)

cc_results_df <- purrr::map2_dfr(.x = cc_results,
                                             .y = i, ~{
                                               .x$Scenario <- .y 
                                               .x$Year <- 1:1000
                                               return(.x)
                                             })

N_i <- ggplot(data = filter(cc_results_df),
       aes(x = Year,
           y = N_i, 
           colour = Scenario))+
  geom_line()

N_j <- ggplot(data = filter(cc_results_df),
              aes(x = Year,
                  y = N_j, 
                  colour = Scenario))+
  geom_line()

N_i + N_j


#### MUTUALISM ####

# Aim is for stability over 1000 years

r <- c(1, 1) # intrinsic growth rates
K <- c(log(50), log(50)) # Carrying capacities
a_joni <- -0.1 # effect of j on i
a_ionj <- -0.3 # effect of i on j
c_i <- 1+(-r[1]/K[1])
c_j <- 1+(-r[2]/K[2]) # calculate intra-specific competition
tau <- 0.05
corr <- 0.7
rho <- corr*tau # correlation = rho/tau
alpha_ionj <- (-r[1]*a_ionj)/K[1] # calculate inter-specific effects
alpha_joni <- (-r[2]*a_joni)/K[2]

alphas <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                 ncol = 2,
                 byrow = TRUE)

starts <- c(100,100)
n = 1000

mm_results <- purrr::rerun(100, 
                           Simulation_func(n = n,
                                           starts = log(starts),
                                           r = r,
                                           alphas = alphas,
                                           tau = tau, 
                                           rho = rho,
                                           burnin = 0)) 

i <- as.list(1:100)

mm_results_df <- purrr::map2_dfr(.x = mm_results,
                                 .y = i, ~{
                                   .x$Scenario <- .y 
                                   .x$Year <- 1:1000
                                   return(.x)
                                 })

N_i <- ggplot(data = filter(mm_results_df),
              aes(x = Year,
                  y = N_i, 
                  colour = Scenario))+
  geom_line()

N_j <- ggplot(data = filter(mm_results_df),
              aes(x = Year,
                  y = N_j, 
                  colour = Scenario))+
  geom_line()

N_i + N_j

#### PREDATOR PREY ####

# Aim is for stability over 1000 years

r <- c(1, 1) # intrinsic growth rates
K <- c(log(50), log(100)) # Carrying capacities
a_joni <- -0.25 # effect of j on i
a_ionj <- 0.25 # effect of i on j
c_i <- 1+(-r[1]/K[1])
c_j <- 1+(-r[2]/K[2]) # calculate intra-specific competition
tau <- 0.05
corr <- 0.7
rho <- corr*tau # correlation = rho/tau
alpha_ionj <- (-r[1]*a_ionj)/K[1] # calculate inter-specific effects
alpha_joni <- (-r[2]*a_joni)/K[2]

alphas <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                 ncol = 2,
                 byrow = TRUE)

starts <- c(100,100)
n = 1000

pp_results <- purrr::rerun(100, 
                           Simulation_func(n = n,
                                           starts = log(starts),
                                           r = r,
                                           alphas = alphas,
                                           tau = tau, 
                                           rho = rho,
                                           burnin = 0)) 

i <- as.list(1:100)

pp_results_df <- purrr::map2_dfr(.x = pp_results,
                                 .y = i, ~{
                                   .x$Scenario <- .y 
                                   .x$Year <- 1:1000
                                   return(.x)
                                 })

N_i <- ggplot(data = filter(pp_results_df),
              aes(x = Year,
                  y = N_i, 
                  colour = Scenario))+
  geom_line()

N_j <- ggplot(data = filter(pp_results_df),
              aes(x = Year,
                  y = N_j, 
                  colour = Scenario))+
  geom_line()

N_i + N_j


##############

