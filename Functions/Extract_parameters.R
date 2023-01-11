# This function extracts the parameter estimates from fitted model 
# does this in format needed for simulation code

# Then runs the simulation

###############################################################################

## Arguments

# Results = results from all models for a particular scenario
# Should be split by interaction type and model type before going into function
# Also list so each individual model results are single entry

###############################################################################

## Function

Extract_parameters <- function(Results,
                               n = 100,
                               starts = NULL,
                               burnin = 50,
                               seed = NULL,
                               maxiter = 50,
                               prevent_extinction = TRUE,
                               NI = FALSE){
  
# parameters that we need are:

# r (vector length 2), tau (can be vector of 2), rho (can be vector of 2)
# also a matrix of alpha values c_i, alpha_ionj, alpha_joni, c_j

## Extract the parameters

r <- c(Results$Mean[Results$label == "r_i"],
       Results$Mean[Results$label == "r_j"])

c <- c(Results$Mean[Results$label == "c_i"],
       Results$Mean[Results$label == "c_j"])

tau <- c(Results$Mean[Results$label == "tau_i"],
         Results$Mean[Results$label == "tau_j"])

if(Results$model[1] == "nimble"){
rho <- c(Results$Mean[Results$label == "prec[2, 1]"],
           Results$Mean[Results$label == "prec[1, 2]"])}else{
rho <- c(Results$Mean[Results$label == "Rho"], 
         Results$Mean[Results$label == "Rho"])       
           }
if(NI == FALSE){
alpha_matrix <- matrix(c(c[1], Results$Mean[Results$label == "alpha_ionj"],
                         Results$Mean[Results$label == "alpha_joni"], c[2]), 
                       ncol = 2, byrow = TRUE)}
if(NI == TRUE){
  alpha_matrix <- matrix(c(c[1], 0,
                           0, c[2]), 
                         ncol = 2, byrow = TRUE)}

## Run the simulation 100 times

Output <- purrr::rerun(100, Simulation_func(n = n, starts = starts, r = r,
                alphas = alpha_matrix,
                tau = tau,
                rho = rho, 
                burnin = burnin,
                seed = seed,
                maxiter = maxiter,
                prevent_extinction = prevent_extinction))

## Output simulation results
return(Output)

}