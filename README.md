# Interspecific-interactions

## Introduction to the project

This repository holds all of the code for a simulation study into the quantification of interspecific interactions from time series data.

In this project I will look at three types of interaction:

- Predator/prey (-/+)
- Competitive (-/-)
- Mutualist (+/+)

## Details of the study

The simulation study has four steps:

1) Generate simulations of species time series data
2) Run a Nimble model for the simulated data
3) Run an INLA model for the simulated data
4) Evaluate the model performances

More detail on each step (including the scripts required to run it) are given below.

### Generate simulations of time series data

This is done in the [Simulation_creation.R]() script. 

I simultaneously simulate two time series of observed population size ($Y$) for species $i$ and species $j$ and the log population size. 

To do this, I use the following equations (Gompertz equations on the log-linear scale):

$Y_i$ ~ Pois(exp($N_i$))
$N_i$ = $r_i$ + $c_i N_i$ + $/alpha_ij N_j$ + $e_i$

$Y_j$ ~ Pois(exp($N_j$))
$N_j$ = $r_j$ + $c_j N_j$ + $/alpha_ji N_i$ + $e_j$

where, $N$ = log actual population size, $r$ = intrinsic growth rate, $c$ = intraspecific competition (density dependence), $/alpha$ = interspecific effect, $e$ = random error. 

Both the true population size and the observed population size including Poisson error are output. 

I simulate populations using the following baseline parameter values:

- starting population size = 100
- $r_i$ = $r_j$ = 1.25
- $c_i$ = $c_j$ = 0.74

The values of alpha depend on the type of interaction:

- Predator/prey: $alpha_ij$ = -0.16, $alpha_ji$ = 0.16
- Competitive: $alpha_ij$ = $alpha_ji$ = 0.13
- Mutulaist: $alpha_ij$ = $alpha_ji$ = -0.18


