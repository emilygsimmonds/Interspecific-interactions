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

#### BASELINE SCENARIO 0.5 cc, -0.1 mm, 0.25 pp strength ####

r <- c(1, 1) # intrinsic growth rates
K <- log(100) # Carrying capacities
a_joni <- -0.5 # effect of j on i
a_ionj <- -0.5 # effect of i on j
c_i <- 1+(-r[1]/K)
c_j <- 1+(-r[2]/K) # calculate intra-specific competition
tau <- 0.05 
corr <- 0.7
rho <- corr*tau # correlation = rho/tau

# make a dataframe of true parameters

true_baseline <- data.frame(r_i = rep(1, 300),
                   r_j = rep(1, 300),
                   c_i = rep(c_i, 300),
                   c_j = rep(c_j, 300),
                   alpha_ionj = rep(NA, 300),
                   alpha_joni = rep(NA, 300),
                   tau_i = rep(tau, 300),
                   tau_j = rep(tau, 300),
                   Rho = rep(rho, 300),
                   interaction = c(rep("m", 100),
                                   rep("p", 100),
                                   rep("c", 100)),
                   scenario = "Baseline")

# MUTUALISM - negative to begin

r <- c(1, 1) # intrinsic growth rates
K <- log(50) # Carrying capacities
a_joni <- -0.1 # effect of j on i
a_ionj <- -0.1 # effect of i on j
c_i <- 1+(-r[1]/K)
c_j <- 1+(-r[2]/K) # calculate intra-specific competition
true_baseline$alpha_ionj[1:100] <- alpha_ionj <- (-r[1]*a_ionj)/K # calculate inter-specific effects
true_baseline$alpha_joni[1:100] <- alpha_joni <- (-r[2]*a_joni)/K

# now combine c and alphas into a single matrix
alphas_m <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                 ncol = 2,
                 byrow = TRUE)

# Create other inputs for Simulation_func

n <- 50
starts <- c(100,100)
tau <- 0.05
corr <- 0.7
rho <- corr*tau # correlation = rho/tau

simulations_output_m <- purrr::rerun(100, 
                                  Simulation_func(n = n,
                                  starts = log(starts),
                                  r = r,
                                  alphas = alphas_m,
                                  tau=tau, 
                                  rho = rho,
                                  burnin = 50)) 
# PREDATOR-PREY

r <- c(1, 1) # intrinsic growth rates
K <- c(log(50), log(100)) # Carrying capacities
a_joni <- -0.25 # effect of j on i
a_ionj <- 0.25 # effect of i on j
c_i <- 1+(-r[1]/K[1])
c_j <- 1+(-r[2]/K[2]) # calculate intra-specific competition
tau <- 0.05
corr <- 0.7
rho <- corr*tau # correlation = rho/tau
true_baseline$alpha_ionj[101:200] <- alpha_ionj <- (-r[1]*a_ionj)/K[1] # calculate inter-specific effects
true_baseline$alpha_joni[101:200] <- alpha_joni <- (-r[2]*a_joni)/K[2]

# now combine c and alphas into a single matrix
alphas_p <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                 ncol = 2,
                 byrow = TRUE)

# Create other inputs for Simulation_func

n <- 50
starts <- c(100,100)
corr <- 0.7
rho <- corr*tau # correlation = rho/tau

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

r <- c(1, 1) # intrinsic growth rates
K <- log(100) # Carrying capacities
a_joni <- 0.5 # effect of j on i
a_ionj <- 0.5 # effect of i on j
c_i <- 1+(-r[1]/K)
c_j <- 1+(-r[2]/K) # calculate intra-specific competition
true_baseline$alpha_ionj[201:300] <- alpha_ionj <- (-r[1]*a_ionj)/K # calculate inter-specific effects
true_baseline$alpha_joni[201:300] <- alpha_joni <- (-r[2]*a_joni)/K

# now combine c and alphas into a single matrix
alphas_c <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                 ncol = 2,
                 byrow = TRUE)

# Create other inputs for Simulation_func

n <- 50
starts <- c(100,100)
tau <- 0.05
corr <- 0.7
rho <- corr*tau # correlation = rho/tau

simulations_output_c <- purrr::rerun(100, 
                                     Simulation_func(n = n,
                                                     starts = log(starts),
                                                     r = r,
                                                     alphas = alphas_c,
                                                     tau=tau, 
                                                     rho = rho,
                                                     burnin = 50,
                                                     maxiter = 50)) 


#### Combine and save ####

simulations_all <- c(simulations_output_c, 
                     simulations_output_m, 
                     simulations_output_p)

save(simulations_all, file="simulated_TS_2021.RData")


###############################################################################

#### UNEQUAL SCENARIO cc (0.3, 0.1), pp (-0.1, 0.3), mm (-0.1, -0.3) ####

r <- c(1, 1) # intrinsic growth rates
K <- log(100) # Carrying capacities
c_i <- 1+(-r[1]/K)
c_j <- 1+(-r[2]/K) # calculate intra-specific competition
tau <- 0.05 
corr <- 0.7
rho <- corr*tau # correlation = rho/tau

true_unequal <- data.frame(r_i = rep(1, 300),
                            r_j = rep(1, 300),
                            c_i = rep(c_i, 300),
                            c_j = rep(c_j, 300),
                            alpha_ionj = rep(NA, 300),
                            alpha_joni = rep(NA, 300),
                            tau_i = rep(tau, 300),
                            tau_j = rep(tau, 300),
                            Rho = rep(rho, 300),
                            interaction = c(rep("m", 100),
                                            rep("p", 100),
                                            rep("c", 100)),
                            scenario = "Unequal")

# MUTUALISM - negative to begin

r <- c(1, 1) # intrinsic growth rates
K <- log(50) # Carrying capacities
a_joni <- -0.1 # effect of j on i
a_ionj <- -0.3 # effect of i on j
c_i <- 1+(-r[1]/K)
c_j <- 1+(-r[2]/K) # calculate intra-specific competition
true_unequal$alpha_ionj[1:100] <- alpha_ionj <- (-r[1]*a_ionj)/K # calculate inter-specific effects
true_unequal$alpha_joni[1:100] <- alpha_joni <- (-r[2]*a_joni)/K

# now combine c and alphas into a single matrix
alphas_m <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                   ncol = 2,
                   byrow = TRUE)

# Create other inputs for Simulation_func

n <- 50
starts <- c(100,100)
corr <- 0.7
rho <- corr*tau # correlation = rho/tau

simulations_output_m <- purrr::rerun(100, 
                                     Simulation_func(n = n,
                                                     starts = log(starts),
                                                     r = r,
                                                     alphas = alphas_m,
                                                     tau=tau, 
                                                     rho = rho,
                                                     burnin = 50)) 
# PREDATOR-PREY

r <- c(1, 1) # intrinsic growth rates
K <- c(log(50), log(100)) # Carrying capacities
a_joni <- -0.1 # effect of j on i
a_ionj <- 0.3 # effect of i on j
c_i <- 1+(-r[1]/K[1])
c_j <- 1+(-r[2]/K[2]) # calculate intra-specific competition
tau <- 0.05
corr <- 0.7
rho <- corr*tau # correlation = rho/tau
true_unequal$alpha_ionj[101:200] <- alpha_ionj <- (-r[1]*a_ionj)/K[1] # calculate inter-specific effects
true_unequal$alpha_joni[101:200] <- alpha_joni <- (-r[2]*a_joni)/K[2]

# now combine c and alphas into a single matrix
alphas_p <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                   ncol = 2,
                   byrow = TRUE)

# Create other inputs for Simulation_func

n <- 50
starts <- c(100,100)
corr <- 0.7
rho <- corr*tau # correlation = rho/tau

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

r <- c(1, 1) # intrinsic growth rates
K <- log(100) # Carrying capacities
a_joni <- 0.3 # effect of j on i
a_ionj <- 0.1 # effect of i on j
c_i <- 1+(-r[1]/K)
c_j <- 1+(-r[2]/K) # calculate intra-specific competition
true_unequal$alpha_ionj[201:300] <- alpha_ionj <- (-r[1]*a_ionj)/K # calculate inter-specific effects
true_unequal$alpha_joni[201:300] <- alpha_joni <- (-r[2]*a_joni)/K

# now combine c and alphas into a single matrix
alphas_c <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                   ncol = 2,
                   byrow = TRUE)

# Create other inputs for Simulation_func

n <- 50
starts <- c(100,100)
corr <- 0.7
rho <- corr*tau # correlation = rho/tau

simulations_output_c <- purrr::rerun(100, 
                                     Simulation_func(n = n,
                                                     starts = log(starts),
                                                     r = r,
                                                     alphas = alphas_c,
                                                     tau=tau, 
                                                     rho = rho,
                                                     burnin = 50,
                                                     maxiter = 2000)) 


#### Combine and save ####

simulations_all <- c(simulations_output_c, 
                     simulations_output_m, 
                     simulations_output_p)

save(simulations_all, file="simulated_TS_UNEQUAL_2021.RData")


###############################################################################

#### INCREASED NOISE1 tau = 0.1, strength = equal ####

r <- c(1, 1) # intrinsic growth rates
K <- log(50) # Carrying capacities
a_joni <- -0.5 # effect of j on i
a_ionj <- -0.5 # effect of i on j
c_i <- 1+(-r[1]/K)
c_j <- 1+(-r[2]/K) # calculate intra-specific competition
true_baseline$alpha_ionj[1:100] <- alpha_ionj <- (-r[1]*a_ionj)/K # calculate inter-specific effects
true_baseline$alpha_joni[1:100] <- alpha_joni <- (-r[2]*a_joni)/K

# now combine c and alphas into a single matrix
alphas_m <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                   ncol = 2,
                   byrow = TRUE)

tau <- 0.1 
corr <- 0.7
rho <- corr*tau # correlation = rho/tau

true_noise1 <- data.frame(r_i = rep(1, 300),
                           r_j = rep(1, 300),
                           c_i = rep(c_i, 300),
                           c_j = rep(c_j, 300),
                           alpha_ionj = true_baseline$alpha_ionj,
                           alpha_joni = true_baseline$alpha_joni,
                           tau_i = rep(tau, 300),
                           tau_j = rep(tau, 300),
                           Rho = rep(rho, 300),
                           interaction = c(rep("m", 100),
                                           rep("p", 100),
                                           rep("c", 100)),
                           scenario = "Noise1")

# MUTUALISM - negative to begin

r <- c(1, 1) # intrinsic growth rates
K <- log(50) # Carrying capacities
a_joni <- -0.1 # effect of j on i
a_ionj <- -0.1 # effect of i on j
c_i <- 1+(-r[1]/K)
c_j <- 1+(-r[2]/K) # calculate intra-specific competition
true_baseline$alpha_ionj[1:100] <- alpha_ionj <- (-r[1]*a_ionj)/K # calculate inter-specific effects
true_baseline$alpha_joni[1:100] <- alpha_joni <- (-r[2]*a_joni)/K

# now combine c and alphas into a single matrix
alphas_m <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                   ncol = 2,
                   byrow = TRUE)

# Create other inputs for Simulation_func

n <- 50
starts <- c(100,100)
rho <- corr*tau # correlation = rho/tau

simulations_output_m <- purrr::rerun(100, 
                                     Simulation_func(n = n,
                                                     starts = log(starts),
                                                     r = r,
                                                     alphas = alphas_m,
                                                     tau=tau, 
                                                     rho = rho,
                                                     burnin = 50)) 
# PREDATOR-PREY

K <- c(log(50), log(100)) # Carrying capacities
a_joni <- -0.25 # effect of j on i
a_ionj <- 0.25 # effect of i on j
c_i <- 1+(-r[1]/K[1])
c_j <- 1+(-r[2]/K[2]) # calculate intra-specific competition
alpha_ionj <- (-r[1]*a_ionj)/K[1] # calculate inter-specific effects
alpha_joni <- (-r[2]*a_joni)/K[2]

# now combine c and alphas into a single matrix
alphas_p <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                   ncol = 2,
                   byrow = TRUE)

# Create other inputs for Simulation_func

n <- 50
starts <- c(100,100)

simulations_output_p <- purrr::rerun(100, 
                                     Simulation_func(n = n,
                                                     starts = log(starts),
                                                     r = r,
                                                     alphas = alphas_p,
                                                     tau = tau, 
                                                     rho = rho,
                                                     burnin = 50,
                                                     maxiter = 2000)) 

# COMPETITION

r <- c(1, 1) # intrinsic growth rates
K <- log(100) # Carrying capacities
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

simulations_output_c <- purrr::rerun(100, 
                                     Simulation_func(n = n,
                                                     starts = log(starts),
                                                     r = r,
                                                     alphas = alphas_c,
                                                     tau=tau, 
                                                     rho = rho,
                                                     burnin = 50,
                                                     maxiter = 500)) 


#### Combine and save ####

simulations_all <- c(simulations_output_c, 
                     simulations_output_m, 
                     simulations_output_p)

save(simulations_all, file="simulated_TS_NOISE1_2021.RData")


###############################################################################

#### INCREASED NOISE2 tau = 0.2, strength = unequal ####

r <- c(1, 1) # intrinsic growth rates
K <- log(50) # Carrying capacities
a_joni <- -0.5 # effect of j on i
a_ionj <- -0.5 # effect of i on j
c_i <- 1+(-r[1]/K)
c_j <- 1+(-r[2]/K) # calculate intra-specific competition
true_baseline$alpha_ionj[1:100] <- alpha_ionj <- (-r[1]*a_ionj)/K # calculate inter-specific effects
true_baseline$alpha_joni[1:100] <- alpha_joni <- (-r[2]*a_joni)/K

# now combine c and alphas into a single matrix
alphas_m <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                   ncol = 2,
                   byrow = TRUE)

tau <- 0.2 
corr <- 0.7
rho <- corr*tau # correlation = rho/tau

true_noise2 <- data.frame(r_i = rep(1, 300),
                          r_j = rep(1, 300),
                          c_i = rep(c_i, 300),
                          c_j = rep(c_j, 300),
                          alpha_ionj = true_baseline$alpha_ionj,
                          alpha_joni = true_baseline$alpha_joni,
                          tau_i = rep(tau, 300),
                          tau_j = rep(tau, 300),
                          Rho = rep(rho, 300),
                          interaction = c(rep("m", 100),
                                          rep("p", 100),
                                          rep("c", 100)),
                          scenario = "Noise2")

# MUTUALISM - negative to begin

r <- c(1, 1) # intrinsic growth rates
K <- log(50) # Carrying capacities
a_joni <- -0.1 # effect of j on i
a_ionj <- -0.1 # effect of i on j
c_i <- 1+(-r[1]/K)
c_j <- 1+(-r[2]/K) # calculate intra-specific competition
true_baseline$alpha_ionj[1:100] <- alpha_ionj <- (-r[1]*a_ionj)/K # calculate inter-specific effects
true_baseline$alpha_joni[1:100] <- alpha_joni <- (-r[2]*a_joni)/K

# now combine c and alphas into a single matrix
alphas_m <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                   ncol = 2,
                   byrow = TRUE)

# Create other inputs for Simulation_func

n <- 50
starts <- c(100,100)
rho <- corr*tau # correlation = rho/tau

simulations_output_m <- purrr::rerun(100, 
                                     Simulation_func(n = n,
                                                     starts = log(starts),
                                                     r = r,
                                                     alphas = alphas_m,
                                                     tau=tau, 
                                                     rho = rho,
                                                     burnin = 50)) 
# PREDATOR-PREY

K <- c(log(50), log(100)) # Carrying capacities
a_joni <- -0.25 # effect of j on i
a_ionj <- 0.25 # effect of i on j
c_i <- 1+(-r[1]/K[1])
c_j <- 1+(-r[2]/K[2]) # calculate intra-specific competition
alpha_ionj <- (-r[1]*a_ionj)/K[1] # calculate inter-specific effects
alpha_joni <- (-r[2]*a_joni)/K[2]

# now combine c and alphas into a single matrix
alphas_p <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                   ncol = 2,
                   byrow = TRUE)

# Create other inputs for Simulation_func

n <- 50
starts <- c(100,100)

simulations_output_p <- purrr::rerun(100, 
                                     Simulation_func(n = n,
                                                     starts = log(starts),
                                                     r = r,
                                                     alphas = alphas_p,
                                                     tau = tau, 
                                                     rho = rho,
                                                     burnin = 50,
                                                     maxiter = 2000)) 

# COMPETITION

r <- c(1, 1) # intrinsic growth rates
K <- log(100) # Carrying capacities
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

simulations_output_c <- purrr::rerun(100, 
                                     Simulation_func(n = n,
                                                     starts = log(starts),
                                                     r = r,
                                                     alphas = alphas_c,
                                                     tau=tau, 
                                                     rho = rho,
                                                     burnin = 50,
                                                     maxiter = 500)) 

#### Combine and save ####

simulations_all <- c(simulations_output_c, 
                     simulations_output_m, 
                     simulations_output_p)

save(simulations_all, file="simulated_TS_NOISE2_2021.RData")




###############################################################################

#### CHANGED CORRELATION1 corr = 0.9 ####

r <- c(1, 1) # intrinsic growth rates
K <- log(100) # Carrying capacities
c_i <- 1+(-r[1]/K)
c_j <- 1+(-r[2]/K) # calculate intra-specific competition

tau <- 0.05 
corr <- 0.9
rho <- corr*tau # correlation = rho/tau

true_corr1 <- data.frame(r_i = rep(1, 300),
                          r_j = rep(1, 300),
                          c_i = rep(c_i, 300),
                          c_j = rep(c_j, 300),
                          alpha_ionj = true_unequal$alpha_ionj,
                          alpha_joni = true_unequal$alpha_joni,
                          tau_i = rep(tau, 300),
                          tau_j = rep(tau, 300),
                          Rho = rep(rho, 300),
                          interaction = c(rep("m", 100),
                                          rep("p", 100),
                                          rep("c", 100)),
                          scenario = "Corr1")


# MUTUALISM - negative to begin

r <- c(1, 1) # intrinsic growth rates
K <- log(50) # Carrying capacities
a_joni <- -0.1 # effect of j on i
a_ionj <- -0.3 # effect of i on j
c_i <- 1+(-r[1]/K)
c_j <- 1+(-r[2]/K) # calculate intra-specific competition
true_unequal$alpha_ionj[1:100] <- alpha_ionj <- (-r[1]*a_ionj)/K # calculate inter-specific effects
true_unequal$alpha_joni[1:100] <- alpha_joni <- (-r[2]*a_joni)/K

# now combine c and alphas into a single matrix
alphas_m <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                   ncol = 2,
                   byrow = TRUE)

# Create other inputs for Simulation_func

n <- 50
starts <- c(100,100)

simulations_output_m <- purrr::rerun(100, 
                                     Simulation_func(n = n,
                                                     starts = log(starts),
                                                     r = r,
                                                     alphas = alphas_m,
                                                     tau=tau, 
                                                     rho = rho,
                                                     burnin = 50, 
                                                     maxiter = 100)) 
# PREDATOR-PREY

r <- c(1, 1) # intrinsic growth rates
K <- c(log(50), log(100)) # Carrying capacities
a_joni <- -0.1 # effect of j on i
a_ionj <- 0.3 # effect of i on j
c_i <- 1+(-r[1]/K[1])
c_j <- 1+(-r[2]/K[2]) # calculate intra-specific competition
tau <- 0.05
corr <- 0.7
rho <- corr*tau # correlation = rho/tau
true_unequal$alpha_ionj[101:200] <- alpha_ionj <- (-r[1]*a_ionj)/K[1] # calculate inter-specific effects
true_unequal$alpha_joni[101:200] <- alpha_joni <- (-r[2]*a_joni)/K[2]

# now combine c and alphas into a single matrix
alphas_p <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                   ncol = 2,
                   byrow = TRUE)

# Create other inputs for Simulation_func

n <- 50
starts <- c(100,100)

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

r <- c(1, 1) # intrinsic growth rates
K <- log(100) # Carrying capacities
a_joni <- 0.3 # effect of j on i
a_ionj <- 0.1 # effect of i on j
c_i <- 1+(-r[1]/K)
c_j <- 1+(-r[2]/K) # calculate intra-specific competition
true_unequal$alpha_ionj[201:300] <- alpha_ionj <- (-r[1]*a_ionj)/K # calculate inter-specific effects
true_unequal$alpha_joni[201:300] <- alpha_joni <- (-r[2]*a_joni)/K

# now combine c and alphas into a single matrix
alphas_c <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                   ncol = 2,
                   byrow = TRUE)

# Create other inputs for Simulation_func

n <- 50
starts <- c(100,100)

simulations_output_c <- purrr::rerun(100, 
                                     Simulation_func(n = n,
                                                     starts = log(starts),
                                                     r = r,
                                                     alphas = alphas_c,
                                                     tau = tau, 
                                                     rho = rho,
                                                     burnin = 50,
                                                     maxiter = 5000)) 


#### Combine and save ####

simulations_all <- c(simulations_output_c, 
                     simulations_output_m, 
                     simulations_output_p)

save(simulations_all, file="simulated_TS_CORR1_2021.2.RData")


###############################################################################

#### CHANGED CORRELATION2 corr = 0.5 ####


r <- c(1, 1) # intrinsic growth rates
K <- log(100) # Carrying capacities
c_i <- 1+(-r[1]/K)
c_j <- 1+(-r[2]/K) # calculate intra-specific competition
tau <- 0.05 
corr <- 0.5
rho <- corr*tau # correlation = rho/tau

true_corr2 <- data.frame(r_i = rep(1, 300),
                         r_j = rep(1, 300),
                         c_i = rep(c_i, 300),
                         c_j = rep(c_j, 300),
                         alpha_ionj = true_unequal$alpha_ionj,
                         alpha_joni = true_unequal$alpha_joni,
                         tau_i = rep(tau, 300),
                         tau_j = rep(tau, 300),
                         Rho = rep(rho, 300),
                         interaction = c(rep("m", 100),
                                         rep("p", 100),
                                         rep("c", 100)),
                         scenario = "Corr2")


# MUTUALISM - negative to begin

r <- c(1, 1) # intrinsic growth rates
K <- log(50) # Carrying capacities
a_joni <- -0.1 # effect of j on i
a_ionj <- -0.3 # effect of i on j
c_i <- 1+(-r[1]/K)
c_j <- 1+(-r[2]/K) # calculate intra-specific competition
true_unequal$alpha_ionj[1:100] <- alpha_ionj <- (-r[1]*a_ionj)/K # calculate inter-specific effects
true_unequal$alpha_joni[1:100] <- alpha_joni <- (-r[2]*a_joni)/K

# now combine c and alphas into a single matrix
alphas_m <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                   ncol = 2,
                   byrow = TRUE)

# Create other inputs for Simulation_func

n <- 50
starts <- c(100,100)

simulations_output_m <- purrr::rerun(100, 
                                     Simulation_func(n = n,
                                                     starts = log(starts),
                                                     r = r,
                                                     alphas = alphas_m,
                                                     tau=tau, 
                                                     rho = rho,
                                                     burnin = 50, 
                                                     maxiter = 100)) 
# PREDATOR-PREY

r <- c(1, 1) # intrinsic growth rates
K <- c(log(50), log(100)) # Carrying capacities
a_joni <- -0.1 # effect of j on i
a_ionj <- 0.3 # effect of i on j
c_i <- 1+(-r[1]/K[1])
c_j <- 1+(-r[2]/K[2]) # calculate intra-specific competition
tau <- 0.05
corr <- 0.7
rho <- corr*tau # correlation = rho/tau
true_unequal$alpha_ionj[101:200] <- alpha_ionj <- (-r[1]*a_ionj)/K[1] # calculate inter-specific effects
true_unequal$alpha_joni[101:200] <- alpha_joni <- (-r[2]*a_joni)/K[2]

# now combine c and alphas into a single matrix
alphas_p <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                   ncol = 2,
                   byrow = TRUE)

# Create other inputs for Simulation_func

n <- 50
starts <- c(100,100)

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

r <- c(1, 1) # intrinsic growth rates
K <- log(100) # Carrying capacities
a_joni <- 0.3 # effect of j on i
a_ionj <- 0.1 # effect of i on j
c_i <- 1+(-r[1]/K)
c_j <- 1+(-r[2]/K) # calculate intra-specific competition
true_unequal$alpha_ionj[201:300] <- alpha_ionj <- (-r[1]*a_ionj)/K # calculate inter-specific effects
true_unequal$alpha_joni[201:300] <- alpha_joni <- (-r[2]*a_joni)/K

# now combine c and alphas into a single matrix
alphas_c <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                   ncol = 2,
                   byrow = TRUE)

# Create other inputs for Simulation_func

n <- 50
starts <- c(100,100)

simulations_output_c <- purrr::rerun(100, 
                                     Simulation_func(n = n,
                                                     starts = log(starts),
                                                     r = r,
                                                     alphas = alphas_c,
                                                     tau=tau, 
                                                     rho = rho,
                                                     burnin = 50,
                                                     maxiter = 2000)) 


#### Combine and save ####

simulations_all <- c(simulations_output_c, 
                     simulations_output_m, 
                     simulations_output_p)

save(simulations_all, file="simulated_TS_CORR2_2021.2.RData")


###############################################################################

#### CHANGED CORRELATION1 corr = 0.3 ####


r <- c(1, 1) # intrinsic growth rates
K <- log(100) # Carrying capacities
c_i <- 1+(-r[1]/K)
c_j <- 1+(-r[2]/K) # calculate intra-specific competition
tau <- 0.05 
corr <- 0.3
rho <- corr*tau # correlation = rho/tau

true_corr3 <- data.frame(r_i = rep(1.25, 300),
                         r_j = rep(1.25, 300),
                         c_i = rep(c_i, 300),
                         c_j = rep(c_j, 300),
                         alpha_ionj = true_unequal$alpha_ionj,
                         alpha_joni = true_unequal$alpha_joni,
                         tau_i = rep(tau, 300),
                         tau_j = rep(tau, 300),
                         Rho = rep(rho, 300),
                         interaction = c(rep("m", 100),
                                         rep("p", 100),
                                         rep("c", 100)),
                         scenario = "Corr3")


# MUTUALISM - negative to begin

r <- c(1, 1) # intrinsic growth rates
K <- log(50) # Carrying capacities
a_joni <- -0.1 # effect of j on i
a_ionj <- -0.3 # effect of i on j
c_i <- 1+(-r[1]/K)
c_j <- 1+(-r[2]/K) # calculate intra-specific competition
true_unequal$alpha_ionj[1:100] <- alpha_ionj <- (-r[1]*a_ionj)/K # calculate inter-specific effects
true_unequal$alpha_joni[1:100] <- alpha_joni <- (-r[2]*a_joni)/K

# now combine c and alphas into a single matrix
alphas_m <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                   ncol = 2,
                   byrow = TRUE)

# Create other inputs for Simulation_func

n <- 50
starts <- c(100,100)

simulations_output_m <- purrr::rerun(100, 
                                     Simulation_func(n = n,
                                                     starts = log(starts),
                                                     r = r,
                                                     alphas = alphas_m,
                                                     tau=tau, 
                                                     rho = rho,
                                                     burnin = 50, 
                                                     maxiter = 100)) 
# PREDATOR-PREY

r <- c(1, 1) # intrinsic growth rates
K <- c(log(50), log(100)) # Carrying capacities
a_joni <- -0.1 # effect of j on i
a_ionj <- 0.3 # effect of i on j
c_i <- 1+(-r[1]/K[1])
c_j <- 1+(-r[2]/K[2]) # calculate intra-specific competition
tau <- 0.05
corr <- 0.7
rho <- corr*tau # correlation = rho/tau
true_unequal$alpha_ionj[101:200] <- alpha_ionj <- (-r[1]*a_ionj)/K[1] # calculate inter-specific effects
true_unequal$alpha_joni[101:200] <- alpha_joni <- (-r[2]*a_joni)/K[2]

# now combine c and alphas into a single matrix
alphas_p <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                   ncol = 2,
                   byrow = TRUE)

# Create other inputs for Simulation_func

n <- 50
starts <- c(100,100)

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

r <- c(1, 1) # intrinsic growth rates
K <- log(100) # Carrying capacities
a_joni <- 0.3 # effect of j on i
a_ionj <- 0.1 # effect of i on j
c_i <- 1+(-r[1]/K)
c_j <- 1+(-r[2]/K) # calculate intra-specific competition
true_unequal$alpha_ionj[201:300] <- alpha_ionj <- (-r[1]*a_ionj)/K # calculate inter-specific effects
true_unequal$alpha_joni[201:300] <- alpha_joni <- (-r[2]*a_joni)/K

# now combine c and alphas into a single matrix
alphas_c <- matrix(c(c_i, alpha_ionj, alpha_joni, c_j), 
                   ncol = 2,
                   byrow = TRUE)

# Create other inputs for Simulation_func

n <- 50
starts <- c(100,100)

simulations_output_c <- purrr::rerun(100, 
                                     Simulation_func(n = n,
                                                     starts = log(starts),
                                                     r = r,
                                                     alphas = alphas_c,
                                                     tau=tau, 
                                                     rho = rho,
                                                     burnin = 50,
                                                     maxiter = 2000)) 


#### Combine and save ####

simulations_all <- c(simulations_output_c, 
                     simulations_output_m, 
                     simulations_output_p)

save(simulations_all, file="simulated_TS_CORR3_2021.2.RData")


