# This script runs a Nimble model to quantify inter-specific interaction 
# strength using the simulated time series

###############################################################################

# If you open in RStudio go to Edit > Folding > Collapse all

###############################################################################

# Set up

#### Load required packages ####

library(igraph) # required for Nimble
library(coda) # needed to extract some outputs
library(nimble) # to run the models
library(devtools) # needed to install nimble
library(tictoc) # times code chunks
library(tidyverse) # for data manipulation and plotting
library(data.table) # to manipulate tables
library(furrr) # allows parallel running

#### Load simulated time series ####

load("simulations_output_2020.RData")

###############################################################################

#### Create Nimble model code ####

# the section below write the Nimble model and saves it as nimbleCode:

code <- nimbleCode({
  
  ## priors for hyperparameters
  
  prec[1:M, 1:M] ~ dwish(Rho[1:M,1:M], M) # precision matrix as correlated
  
  ## priors for parameters
  
  # truncated normal used when parameter cannot be < 0
  r_i ~ T(dnorm(1.5, sd = 1), 0, 10) 
  r_j ~ T(dnorm(1.5, sd = 1), 0, 10)
  c_i ~ T(dnorm(0.5, sd = 10), 0, 10) 
  c_j ~ T(dnorm(0.5, sd = 10), 0, 10)
  
  mu[1, 1] ~ dnorm(6, 1) # means of log population size at t+1
  mu[1, 2] ~ dnorm(6, 1)
  
  # actual log population size at t+1
  N[1, 1:M] ~ dmnorm(mu[1, 1:M], prec[1:M,1:M]) 
  
  for(i in 1:(n-1)){
    
    # likelihoods:
    
    # state process
    
    mu[i+1,1:M] <- c(r_i + (c_i*N[i,1])+(alpha_ij*N[i,2]),
                     r_j + (c_j*N[i,2])+(alpha_ji*N[i,1]))
    N[i+1, 1:M] ~ dmnorm(mu[i+1, 1:M], 
                         prec[1:M,1:M])
    
    # observation process
    
    Y_i[i+1] ~ dpois(exp(N[i+1, 1]))
    Y_j[i+1] ~ dpois(exp(N[i+1, 2])) 
  }
  
})

#### Set up inits, constants, and data ####

# CONSTANTS

n <- length(simulations_all[[1]][,1]) # number of years in time series

constants <- list(n=n, M=2)

# INITS

# should set up enough mu's to fill whole vector

inits <- list(r_i = 1, r_j = 1, 
              c_i = 0.5, c_j = 0.5, 
              prec = matrix(c(1,0,0,1),2,2),
              N = matrix(c(6,6),n,2),
              mu = matrix(rep(6,(n*2)),n,2),
              Rho = matrix(c(1,0,0,1),2,2))

# DATA

# set up grid of alpha values

alphas <- as.matrix(expand_grid(alphaij = seq(-1,1,length.out = 5),
                                alphaji = seq(-1,1,length.out = 5)))

data <- list(Y_i = (round(exp(simulations_all[[1]]$Y_i))), 
             Y_j = (round(exp(simulations_all[[1]]$Y_j))),
             alpha_ij = alphas[1,1], alpha_ji = alphas[1,2])

###############################################################################

#### Run the models ####

# create model in R

RmodelS1 <- nimbleModel(code, constants, data, inits)

# check the model 

RmodelS1$initializeInfo() # check everything initialised
RmodelS1$calculate() # calculate likelihood based on initial values

# compile in C

CmodelS1 <- compileNimble(RmodelS1) # this compiles the model in C++
CmodelS1$calculate() # check likelihood is still the same

# set up covariance matrix for parameters (r and c)
# it was identified in early analyses that these are correlated
# setting up covariance allows block sampling

cov1 <- cov2 <- matrix(c(0.003, -0.0015,
                         -0.0015, 0.003), nrow=2, ncol=2, byrow=T)

# configure

confS1 <- configureMCMC(RmodelS1) 

# change samples to block sampler

# remove old

confS1$removeSamplers(c('c_i', 'c_j', 'r_i', 'r_j'))

# add the new ones

confS1$addSampler(target = c('c_j', 'r_j'),
                  type = 'RW_block',
                  propCov = cov1)
confS1$addSampler(target = c('c_i', 'r_i'),
                  type = 'RW_block', 
                  propCov = cov2)

# build sampling algorithm

# build in R

RmcmcS1 <- buildMCMC(confS1)

# compile in C

CmcmcS1 <- compileNimble(mcmc=RmcmcS1)

# split alpha grid to be a list to use in map()

alphas <- split(alphas, seq(nrow(alphas)))

#### Run the model for single scenarios ####

tic() # starts a timer

# Run Nimble model across all alphas for a SINGLE scenario

Scenario1Samples <- map(.x = alphas, ~{
  CmodelS1$setData(list(
    alpha_ij = .x[1], alpha_ji = .x[2]))
  runMCMC(CmcmcS1, 
          niter = 50000, 
          nburnin = 5000,
          thin = 100,
          nchains = 3,
          summary = TRUE, 
          samples = TRUE,
          samplesAsCodaMCMC = TRUE)
}, CmodelS1, CmcmcS1)

toc() # print time taken

###############################################################################

# This code runs the model for many scenarios - but would take ages!

#### Run model for multiple scenarios ####

# make a list of all simulated time series repeated same number as number of 
# alphas

simulationsGrid <- rep(simulations_all, each = 25)

# set up plan for cores

plan(multicore(workers = 2))

tic() # starts a timer

ScenarioSamplesCC <- future_map2(.x = alphasGrid[1:2500], 
                                 .y = simulationsGrid[1:2500], ~{
  CmodelS1$setData(list(Y_i = (round(exp(.y$Y_i))), 
                        Y_j = (round(exp(.y$Y_j))),
                        alpha_ij = .x[1], alpha_ji = .x[2]))
  runMCMC(CmcmcS1, 
          niter = 50000, 
          nburnin = 5000,
          thin = 100,
          nchains = 3,
          summary = TRUE, 
          samples = TRUE,
          samplesAsCodaMCMC = TRUE)
}, CmodelS1, CmcmcS1, .progress=TRUE)

toc() # print time taken

