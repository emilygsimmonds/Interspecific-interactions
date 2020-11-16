# This function iterates the Gompertz function for multiple years

###############################################################################

#### Required packages/libraries ####

# You will need package MASS installed

###############################################################################

#### Source required user built functions ####

# function to simulate a single year of population sizes
source("./Gompertz_function.R")


###############################################################################

#### Define function ####

# Arguments:

  # parameters = data frame with columns:
    # n, start_i, start_j, r_i, r_j, c_i, c_j, alpha_ij, alpha_ji, tau, rho

    # i and j = the interacting species
    # n = length of simulation
    # start = starting log population size
    # r = intrinsic growth rate
    # c = intra-specific competition
    # alpha_ij = effect of species j on species i
    # alpha_ji = effect of species i on species j
    # tau = variance of noise (mean = 0)
    # rho = covariance of noise between species

# Function outputs N = true population size and Y = observed population size

Simulation_func <- function(parameters){
  
  # First define an empty vector of length n to store observed population sizes
  Y_j <- Y_i <- rep(NA, parameters$n)
  
  # and true population size
  N <- matrix(NA, nrow=(parameters$n+100), ncol=2)
  
  # The first values are given in parameters
  N[1,1] <- parameters$start_i
  N[1,2] <- parameters$start_j
  
  # Then simulate the random noise from a multivariate normal
  e <- MASS::mvrnorm(n = parameters$n+100, 
               mu = c(0,0), 
               Sigma = matrix(c(parameters$tau, 
                                parameters$rho, 
                                parameters$rho, 
                                parameters$tau), 2, 2, byrow=T)) 

  # simulate the process of population change on log scale using Gompertz_func
  
  # n+99 years so that the first 100 entries can be burnin
  for(t in 1:(parameters$n+99)){
    N[t+1,] <- exp(Gompertz_func(parameters$r_i,
                           parameters$r_j,
                           parameters$c_i, 
                           parameters$c_j,
                           log(N[t,1]), 
                           log(N[t,2]), 
                           e[t,1], 
                           e[t,2],
                           parameters$alpha_ij,
                           parameters$alpha_ji))
  }
  
  # simulate observation process
  for(t in 100:(99+parameters$n)){
    Y_i[t-99] <- rpois(1, round(N[t,1])) # must be a whole number
    Y_j[t-99] <- rpois(1, round(N[t,2]))
    
  }
  
  # DO NOT want a population to go extinct or too high in numbers
  # next block of code checks that no population sizes simulated are <1 or
  # > than a high number
  checker <- c(Y_i, Y_j, N[100:(99+parameters$n),1], 
               N[100:(99+parameters$n),2])
  test <- length(c(which(checker < 1),which(checker > 100000)))
  
  # If the test shows that any populations did not meet the criteria the 
  # simulation is repeated until the test is passed
  while(test > 0){
    e <- MASS::mvrnorm(n = parameters$n+100, 
                 mu = c(0,0), 
                 Sigma = matrix(c(parameters$tau, 
                                  parameters$rho, 
                                  parameters$rho, 
                                  parameters$tau), 2, 2, byrow=T)) # error term
    for(t in 1:(parameters$n+99)){
      N[t+1,] <- exp(Gompertz_func(parameters$r_i,
                             parameters$r_j,
                             parameters$c_i, 
                             parameters$c_j,
                             log(N[t,1]), 
                             log(N[t,2]), 
                             e[t,1], 
                             e[t,2],
                             parameters$alpha_ij,
                             parameters$alpha_ji))
    }
    for(t in 100:(99+parameters$n)){
      Y_i[t-99] <- rpois(1, round(N[t,1]))
      Y_j[t-99] <- rpois(1, round(N[t,2]))
      
    }
    checker <- c(Y_i, Y_j, N[100:(99+parameters$n),1], 
                 N[100:(99+parameters$n),2])
    test <- length(c(which(checker < 1),which(checker > 100000)))
    print(test)} # show how many years are failing
    
  # Return both observed and true population sizes
  return(data.frame(
    N_i = N[100:(99+parameters$n),1], N_j = N[100:(99+parameters$n),2],
    Y_i = Y_i, Y_j = Y_j))
}
