# Script to run noise scenarios

###############################################################################

#### source user defined functions ####

source("NimbleModelNaive.R")
#source("NimbleModelGridded.R")
source("INLAModel.R")
source("runINLAModel.R")

#### Load simulated time series ####

load("simulated_TS_NOISE2_2021.RData")
simulations_all2 <- simulations_all
load("simulated_TS_NOISE1_2021.RData")

#### run naive model ####

noise1_naive_nimble <- run_naive_nimble_model(simulations_all)

saveRDS(noise1_naive_nimble, file="Results/Noise1_naive_nimble_results_2021.rds")

noise2_naive_nimble <- run_naive_nimble_model(simulations_all2)

saveRDS(noise2_naive_nimble, file="Results/Noise2_naive_nimble_results_2021.rds")

#### run gridded approach model ####

# size = number of different alphas to check 5 gives 25 total combos
#
#for(i in seq(1, 300, 50)){
#  
#  noise1_gridded_nimble <- run_gridded_nimble_model(simulations_all[i:(i+49)], 
#                                                      size = 5)
#  saveRDS(noise1_gridded_nimble, 
#          file=paste0("Results/noise1_gridded_nimble_results", i, 
#                      ".rds"))
#  
#}
#
#for(i in seq(1, 300, 50)){
#  
#  noise2_gridded_nimble <- run_gridded_nimble_model(simulations_all2[i:(i+49)], 
#                                                      size = 5)
#  saveRDS(noise2_gridded_nimble, 
#          file=paste0("Results/noise2_gridded_nimble_results", i, 
#                      ".rds"))
#  
#}

#### run inla model ####

noise1_inla <- run_inla_model_parallel(simulations_all)

saveRDS(noise1_inla, file="Results/Noise1_inla_results_2021.rds")

noise2_inla <- run_inla_model_parallel(simulations_all2)

saveRDS(noise2_inla, file="Results/Noise2_inla_results_2021.rds")
