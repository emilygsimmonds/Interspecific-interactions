# This script runs a Nimble model to quantify inter-specific interaction 
# strength using the simulated time series

# The model is the gridded approach

# runs model for ALL elements of a list of datafiles

###############################################################################

# If you open in RStudio go to Edit > Folding > Collapse all

###############################################################################

source('storeLogProb.R')

run_gridded_nimble_model <- function(simulations_all, size){
  
# Set up

#### Load required packages ####

require(igraph) # required for Nimble
require(coda) # needed to extract some outputs
require(nimble) # to run the models
require(devtools) # needed to install nimble
require(tictoc) # times code chunks
require(tidyverse) # for data manipulation and plotting
require(data.table) # to manipulate tables
require(furrr) # allows parallel running

###############################################################################

alphas <- as.matrix(expand_grid(alphaij = seq(-1,1,length.out = size),
                                alphaji = seq(-1,1,length.out = size)))

## save the code as nimble format
code <- nimbleCode({
  ## priors for hyperparameters
  prec[1:M, 1:M] ~ dwish(Rho[1:M,1:M], M) 
  
  ## priors for parameters
  r_i ~ T(dnorm(1, sd = 1), 0, 10)
  r_j ~ T(dnorm(1, sd = 1), 0, 100)
  c_i ~ T(dnorm(0.5, sd = 10), 0, 10)
  c_j ~ T(dnorm(0.5, sd = 10), 0, 10)
  
  mu[1, 1] ~ dnorm(6, 1)
  mu[1, 2] ~ dnorm(6, 1)
  
  N[1, 1:M] ~ dmnorm(mu[1, 1:M], prec[1:M,1:M])
  
  for(i in 1:(n-1)){
    
    # likelihoods 
    # state process
    mu[i+1,1:M] <- c(r_i + (c_i*N[i,1])+(alpha_ij*N[i,2]),
                     r_j + (c_j*N[i,2])+(alpha_ji*N[i,1]))
    N[i+1, 1:M] ~ dmnorm(mu[i+1, 1:M], 
                         prec[1:M,1:M]) # likelihood for process SPi
    
    # observation process
    Y_i[i+1] ~ dpois(exp(N[i+1, 1]))
    Y_j[i+1] ~ dpois(exp(N[i+1, 2])) 
  }
  
  sumLogProb ~ dnorm(0,1) ## dnorm(0,1) will not be used.  It just establishes sumLogProb as a stochastic node.
  
})



n <- length(simulations_all[[1]][,1])

# constants
constants <- list(n=n, M=2)

# inits
# should set up enough mu's to fill whole vector
inits <- list(r_i = 1, r_j = 1, 
              c_i = 0.5, c_j = 0.5, 
              prec = matrix(c(1,0,0,1),2,2),
              N = matrix(c(6,6),n,2),
              mu = matrix(rep(6,(n*2)),n,2),
              Rho = matrix(c(1,0,0,1),2,2), 
              sumLogProb = 0)

data <- list(Y_i = (round(simulations_all[[1]]$Y_i)), 
             Y_j = (round(simulations_all[[1]]$Y_j)),
             alpha_ij = alphas[1,1], alpha_ji = alphas[1,2])

#### Compile model ####

# create model in R
RmodelS1 <- nimbleModel(code, constants, data, inits)

# compile in C
CmodelS1 <- compileNimble(RmodelS1) # this compiles the model in C++

cov1 <- cov2 <- matrix(c(0.003, -0.0015,
                         -0.0015, 0.003), nrow=2, ncol=2, byrow=T)

confS1 <- configureMCMC(RmodelS1) # configure
confS1$printMonitors()
confS1$addMonitors(c('N'))
configureStoreLogProb(confS1, RmodelS1, 'sumLogProb')
confS1$printMonitors()

confS1$removeSamplers(c('c_i', 'c_j', 'r_i', 'r_j'))
# add the new ones
confS1$addSampler(target = c('c_j', 'r_j'),
                  type = 'RW_block',
                  propCov = cov1)
confS1$addSampler(target = c('c_i', 'r_i'),
                  type = 'RW_block', 
                  propCov = cov2)


RmcmcS1 <- buildMCMC(confS1, enableWAIC = TRUE)

CmcmcS1 <- compileNimble(mcmc=RmcmcS1)

# Run function across list using map()
alphas <- split(alphas, seq(nrow(alphas)))

#### Run model for multiple scenarios ####

# make a list of all simulated time series repeated same number as number of 
# alphas
alphasGrid <- rep(alphas, length(simulations_all))
simulationsGrid <- rep(simulations_all, each =size^2)

# set up plan for cores

plan(multicore(workers = 2))

tic() # starts a timer

ScenarioSamples <- future_map2(.x = alphasGrid, 
                               .y = simulationsGrid, safely(~{
                                 CmodelS1$setData(list(Y_i = (round(.y$Y_i)), 
                                                       Y_j = (round(.y$Y_j)),
                                                       alpha_ij = .x[1], 
                                                       alpha_ji = .x[2]))
                                 runMCMC(CmcmcS1, 
                                         niter = 10000, 
                                         nburnin = 1000,
                                         nchains = 3,
                                         summary = TRUE, 
                                         samples = TRUE,
                                         samplesAsCodaMCMC = TRUE)$result$summary$all.chains
                                 }), CmodelS1, CmcmcS1, .progress=TRUE)

toc() # print time taken

return(ScenarioSamples)

}