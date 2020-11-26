# script to test Gompertz function

###############################################################################

#### source script to test ####

source("Gompertz_function.R")

#### Test 1: does it work? ####

# set sensible parameter values

# arguments are: r (intrinsic growth rates), N (abundances), e (noise), 
# alphas (interactions)

# r, N, e are all vectors with length = number of species
# alphas = matrix with ncol = nrow = number of species

# set them differently to check everything lines up
r <- c(1, 1.25)
N <- c(log(50), log(100))
e <- c(0.1, 0.5)
alpha_ionj <- -0.1
alpha_joni <- 0.1
alphas <- matrix(c(0.7, -0.1, 0.1, 0.8), 
                 ncol = 2,
                 byrow = TRUE) # diagonal = intra-specific effects!!

Gompertz_func(r = r, N = N, e = e, alphas = alphas)

# manual check:

r[1] + (alphas[1,1]*N[1]) + (alpha_joni*N[2]) + e[1]
r[2] + (alphas[2,2]*N[2]) + (alpha_ionj*N[1]) + e[2]

### YAY #### it works!

###############################################################################

#### Does it give the right error? ####

# set r to NULL

r <- NULL

Gompertz_func(r = r, N = N, e = e, alphas = alphas) # YES

r <- c(1, 1.25)

# set N to NULL

N <- NULL 

Gompertz_func(r = r, N = N, e = e, alphas = alphas) # YES

N <- c(log(50), log(100))

# set e to NULL

e <- NULL

Gompertz_func(r = r, N = N, e = e, alphas = alphas) # YES

e <- c(0.1, 0.5)

# set alphas to NULL

alphas <- NULL

Gompertz_func(r = r, N = N, e = e, alphas = alphas) # YES

# set alpha as vector NOT matrix

alphas <- c(0.1,0.1,0.1,0.1)

Gompertz_func(r = r, N = N, e = e, alphas = alphas) # YES

# make one element the wrong length

alphas <- matrix(c(0.7, -0.1, 0.1, 0.8), 
                 ncol = 2,
                 byrow = TRUE) # diagonal = intra-specific effects!!

r <- 1

Gompertz_func(r = r, N = N, e = e, alphas = alphas) # Yes

# what if two vectors are wrong?

N <- 1

Gompertz_func(r = r, N = N, e = e, alphas = alphas) # Yes

