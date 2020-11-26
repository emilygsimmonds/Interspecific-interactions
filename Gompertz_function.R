# This function simulates the next population size for each species using a
# Gompertz equation

###############################################################################

# Arguments:

  # r = intrinsic growth rate (vector of length = number of species)
  # N = log population size (vector of length = number of species)
  # e = random noise (vector of length = number of species)
  # alphas = matrix with ncol = nrow = number of species 

  # matrix is of the form where each row 
  # contains the effects of a focal species on all others inc itself (diagonal)

Gompertz_func <- function(r = NULL, 
                          N = NULL, 
                          e = NULL, 
                          alphas = NULL){
  #### CHECKS
  
  # nothing is missing
  
  if(is.null(r)){stop("please supply a vector for r (intrinsic growth rate)")}
  if(is.null(N)){stop("please supply a vector for N (starting abundance)")}
  if(is.null(e)){stop("please supply a vector for e (noise)")}
  if(is.null(alphas)){stop("please supply a matrix for alphas 
                           (intra/interspecific interactions)")}
  
  # all elements are correct size
  
  if((length(r)==length(N) &
     length(N)==length(e) &
     length(e)==length(alphas)/2)==FALSE)
  {stop("inputs are not the same length")}
  
  if(!is.matrix(alphas)){stop("alpha should be a matrix")}
  
  #### USE THE FUNCTION
  
  # simulate the value of Ni and Nj at t+1
  
  # generalise the equation so it can be used for many species
  
  interactions <- colSums(alphas*N)
  
  N.new <- r + interactions + e # r and e are vectors
  
  # return the new log population size values
  
  return(N.new)
}
