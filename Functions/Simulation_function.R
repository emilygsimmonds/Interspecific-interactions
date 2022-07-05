# This function iterates the Gompertz function for multiple years

###############################################################################

#### Required packages/libraries ####

# You will need package MASS installed

###############################################################################

#### Source required user built functions ####

# function to simulate a single year of population sizes
source("./Functions/Gompertz_function.R")


###############################################################################

#### Define function ####

# Arguments:
  
  # n, starts, r, alphas, tau, rho, burnin

    # n = length of simulation
    # starts = starting log population size for each species
    # r = intrinsic growth rates (vector of length = number of species)
    # alphas = matrix with ncol = nrow = number of species 
    # tau = variance of noise (mean = 0)
    # rho = covariance of noise between species (length = number of species -1)
    # burnin, number of years wanted to discard from simulation (default = 100)

# Function outputs N = true population size and Y = observed population size

Simulation_func <- function(n = 100,
                            starts = NULL,
                            r = NULL,
                            alphas = NULL,
                            tau = NULL,
                            rho = NULL, 
                            burnin = 100,
                            seed = NULL,
                            maxiter = 50,
                            prevent_extinction = TRUE){
  
#### CHECKS

# is anything missing?

if(is.null(starts)){stop("please supply starting pop size (starts)")}
if(is.null(r)){stop("please supply intrinsic growth rate (r)")}
if(is.null(alphas)){stop("please supply interaction strengths (alphas)")}
if(is.null(tau)){stop("please supply var of noise (tau)")}
if(is.null(rho)){stop("please supply correlation of noise (rho)")}

# are all of the elements the correct length?

if((length(r)==length(starts) &
    length(starts)==length(alphas)/2)==FALSE)
{stop("inputs (r, N, starts, e, or half alphas) are not the same length")}

if((length(tau)==length(rho))==FALSE)
{stop("inputs (tau and rho) are not the same length")}
  

# First define an empty vector of length n to store observed population sizes
Y_j <- Y_i <- rep(NA, n)

# and true population size
# n + 1 to allow start values
N <- matrix(NA, nrow=(n+burnin+1), ncol=2)

# The first values are given in parameters
N[1,] <- log(starts)

# Then simulate the random noise from a multivariate normal
if(length(tau) == 1){
e <- MASS::mvrnorm(n = n+burnin, 
             mu = c(0,0), 
             Sigma = matrix(c(tau, 
                              rho, 
                              rho, 
                              tau), 2, 2, byrow=TRUE))}
if(length(tau) == 2){
  e <- MASS::mvrnorm(n = n+burnin, 
                     mu = c(0,0), 
                     Sigma = matrix(c(tau[1], 
                                      rho[1], 
                                      rho[2], 
                                      tau[2]), 2, 2, byrow=TRUE))}


# simulate the process of population change on log scale using Gompertz_func

# the first x entries are burnin
for(t in 2:(n+burnin+1)){
  N[t,] <- Gompertz_func(r = r,
                               N = N[t-1,],
                               e = e[t-1,], 
                               alphas = alphas)
}

# simulate observation process
for(k in 1:n){
  if(!is.null(seed)){set.seed(seed)}
  Y_i[k] <- rpois(1, round(exp(N[(k+burnin+1),1]))) # must be a whole number
  Y_j[k] <- rpois(1, round(exp(N[(k+burnin+1),2])))
  
}

# DO NOT want a population to go extinct or too high in numbers
# next block of code checks that no population sizes simulated are < 1 or
# > than a high number
checker <- c(Y_i, Y_j, N[(2+burnin):(burnin+n+1),1], 
             N[(2+burnin):(burnin+n+1),2])

test <- length(c(which(checker < 1),which(checker > 100000)))

if(prevent_extinction == FALSE){test <- 0}

rep <- 1 # do not want it to continue forever - give 50 chances

# If the test shows that any populations did not meet the criteria the 
# simulation is repeated until the test is passed

while(test > 0 &
      rep < maxiter){
  if(length(tau) == 1){
    e <- MASS::mvrnorm(n = n+burnin, 
                       mu = c(0,0), 
                       Sigma = matrix(c(tau, 
                                        rho, 
                                        rho, 
                                        tau), 2, 2, byrow=TRUE))}
  if(length(tau) == 2){
    e <- MASS::mvrnorm(n = n+burnin, 
                       mu = c(0,0), 
                       Sigma = matrix(c(tau[1], 
                                        rho[1], 
                                        rho[2], 
                                        tau[2]), 2, 2, byrow=TRUE))}
  
  for(t in 2:(n+burnin+1)){
    N[t,] <- Gompertz_func(r = r,
                           N = N[t-1,],
                           e = e[t-1,], 
                           alphas = alphas)
  }

  for(k in 1:n){
    if(!is.null(seed)){set.seed(seed)}
    Y_i[k] <- rpois(1, round(exp(N[(k+burnin+1),1]))) # must be a whole number
    Y_j[k] <- rpois(1, round(exp(N[(k+burnin+1),2])))
  }
  checker <- c(Y_i, Y_j, N[(2+burnin):(burnin+n+1),1], 
               N[(2+burnin):(burnin+n+1),2])
  
  test <- length(c(which(checker < 1),which(checker > 100000)))
  rep <- rep+1} 
  
  if(test > 0){stop("Population went out of bounds")}
  
# Return both observed and true population sizes
return(data.frame(N_i = exp(N[(2+burnin):(burnin+n+1),1]), 
                  N_j = exp(N[(2+burnin):(burnin+n+1),2]),
                  Y_i = Y_i, Y_j = Y_j))
}
