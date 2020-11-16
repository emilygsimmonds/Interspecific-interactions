# This function simulates the next population size for each species using a
# Gompertz equation

###############################################################################

# Arguments:

  # i and j = the interacting species
  # r = intrinsic growth rate
  # c = intra-specific competition
  # N = log population size
  # e = random noise
  # alpha_ij = effect of species j on species i
  # alpha_ji = effect of species i on species j


Gompertz_func <- function(r_i, r_j, 
                          c_i, c_j, 
                          N_i, N_j, 
                          e_i, e_j, 
                          alpha_ij, alpha_ji){
  
  # simulate the value of Ni and Nj at t+1
  
  N.new <- c(r_i + (c_i*N_i) + (alpha_ij*N_j) + e_i,
             r_j + (c_j*N_j) + (alpha_ji*N_i) + e_j)
  
  # return the new log population size values
  
  return(N.new)
}
