###############################################################################

#### DIFFERENT PAPER FORMULATIONS

# set up the test parameters

r <- c(1, 1) # intrinsic growth rates
K <- log(150) # Carrying capacities
e <- c(0.1, 0.1)
N <- c(100, 100)

# parameter values on ORIGINAL SCALE - COMPETITION
alpha_ionj <- 0.5
alpha_joni <- 0.7

alpha_ii <- (-r[1]/K)
alpha_jj <- (-r[2]/K)

#### Check equations on original scale ####

#### Stenseth 2015:

N[1]*exp(r[1] * (1 - (((alpha_ii*log(N[1])) + (alpha_joni*log(N[2])))/K))) + e[1]
N[2]*exp(r[2] * (1 - (((alpha_jj*log(N[2])) + (alpha_ionj*log(N[1])))/K))) + e[2]

#### Mutshinda 2011:

N[1]*exp(r[1] * (1-(sum(c(alpha_ii*log(N[1]),alpha_joni*log(N[2])))/K))) + e[1]
N[2]*exp(r[2] * (1-(sum(c(alpha_jj*log(N[2]),alpha_ionj*log(N[1])))/K))) + e[2]

#### Check equations on log-linear scale ####

#### Stenseth 2015:

c_i <- 1+(-r[1]/K)
c_j <- 1+(-r[2]/K) # calculate intra-specific competition
a_joni <- (-r[1]/K)*alpha_joni
a_ionj <- (-r[2]/K)*alpha_ionj

exp(r[1] + (c_i*log(N[1])) + (a_joni*log(N[2])) + e[1])
exp(r[2] + (c_j*log(N[2])) + (a_ionj*log(N[1])) + e[2])


alphas <- matrix(c(c_i, a_ionj, a_joni, c_j),
                 ncol = 2,
                 byrow = TRUE)

exp(Gompertz_func(r = r, 
              N = log(N), 
              e = e, 
              alphas = alphas))
