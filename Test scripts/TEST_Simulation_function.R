# script to test Simulation function

###############################################################################

#### source script to test ####

source("Simulation_function.R")

###############################################################################

#### Test 1: does it work? ####

# Try with tau = 0 so it is somewhat deterministic

r <- c(1, 1.25)
N <- c(log(100), log(100))
e <- c(0, 0)
burnin <- 1
starts <- c(100, 100)
n <- 1

alpha_ionj <- -0.1
alpha_joni <- 0.1

# matrix format = 
# aii, aionj,
# ajoni, ajj

alphas <- matrix(c(0.7, -0.1, 0.1, 0.7), 
                 ncol = 2,
                 byrow = TRUE) # diagonal = intra-specific effects!!

# for these parameters, would expect answer to be:

answer <- starts

for(i in 1:(n+burnin)){
  
answer <- exp(Gompertz_func(r = r,
                        N = log(answer),
                        e = e,
                        alphas = alphas))
}
  
set.seed(20)

expectation <- c(rpois(1, round(answer[1])),
  rpois(1, round(answer[2])))

result <- Simulation_func(n = n,
                starts = c(100,100),
                r = r,
                alphas = alphas,
                tau = 0,
                rho = 0, 
                burnin = burnin,
                seed = 20)[3:4]

if(expectation[1] == result[1]){"Yay"}
if(expectation[2] == result[2]){"Yay"}

#### Test 2: longer burnin ####

burnin = 10

answer <- starts

for(i in 1:(n+burnin)){
  
  answer <- exp(Gompertz_func(r = r,
                              N = log(answer),
                              e = e,
                              alphas = alphas))
}

set.seed(20)

expectation <- c(rpois(1, round(answer[1])),
                 rpois(1, round(answer[2])))

result <- Simulation_func(n = 1,
                          starts = c(100,100),
                          r = r,
                          alphas = alphas,
                          tau = 0,
                          rho = 0, 
                          burnin = burnin,
                          seed = 20)[3:4]

if(expectation[1] == result[1]){"Yay"}
if(expectation[2] == result[2]){"Yay"}

#### Test 3: longer n ####

n = 10

answer <- starts

for(i in 1:(n+burnin)){
  
  answer <- exp(Gompertz_func(r = r,
                              N = log(answer),
                              e = e,
                              alphas = alphas))
}

set.seed(20)

expectation <- c(rpois(1, round(answer[1])),
                 rpois(1, round(answer[2])))

result <- Simulation_func(n = n,
                          starts = c(100,100),
                          r = r,
                          alphas = alphas,
                          tau = 0,
                          rho = 0, 
                          burnin = burnin,
                          seed = 20)[3:4]

if(expectation[1] == result[n,1]){"Yay"}
if(expectation[2] == result[n,2]){"Yay"}

#### Test 4: correct error messages? ####

# if starts is not provided

Simulation_func(n = n,
                #starts = starts,
                r = r,
                alphas = alphas,
                tau = 0,
                rho = 0, 
                burnin = burnin,
                seed = 20)

# if r is not provided

Simulation_func(n = n,
                starts = starts,
                #r = r,
                alphas = alphas,
                tau = 0,
                rho = 0, 
                burnin = burnin,
                seed = 20)

# if alphas is not provided

Simulation_func(n = n,
                starts = starts,
                r = r,
                #alphas = alphas,
                tau = 0,
                rho = 0, 
                burnin = burnin,
                seed = 20)

# if tau is not provided

Simulation_func(n = n,
                starts = starts,
                r = r,
                alphas = alphas,
                #tau = 0,
                rho = 0, 
                burnin = burnin,
                seed = 20)

# if alphas is not provided

Simulation_func(n = n,
                starts = starts,
                r = r,
                alphas = alphas,
                tau = 0,
                #rho = 0, 
                burnin = burnin,
                seed = 20)

# are all of the elements the correct length?

r <- 1

Simulation_func(n = n,
                starts = starts,
                r = r,
                alphas = alphas,
                tau = 0,
                rho = 0, 
                burnin = burnin,
                seed = 20)

r <- c(1, 1.25)

rho <- c(0, 0)

Simulation_func(n = n,
                starts = starts,
                r = r,
                alphas = alphas,
                tau = 0,
                rho = rho, 
                burnin = burnin,
                seed = 20)
