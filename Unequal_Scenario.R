# Script to run unequal scenario

###############################################################################

#### source user defined functions ####

source("NimbleModelNaive.R")
source("NimbleModelGridded.R")
source("INLAModel.R")
source("runINLAModel.R")

#### Load simulated time series ####

load("simulated_TS_UNEQUAL_2020.RData")

#### run naive model ####

unequal_naive_nimble <- run_naive_nimble_model(simulations_all)

saveRDS(unequal_naive_nimble, file="Unequal_naive_nimble_results.rds")

#### run inla model ####

unequal_inla <- run_inla_model_parallel(simulations_all)

saveRDS(unequal_inla, file="Unequal_inla_results.rds")

#### run gridded approach model ####

# size = number of different alphas to check 5 gives 25 total combos

for(i in seq(1, 300, 50)){
  
  unequal_gridded_nimble <- run_gridded_nimble_model(simulations_all[i:(i+49)], 
                                                     size = 5)
  
  saveRDS(unequal_gridded_nimble, 
          file=paste0("Results/Unequal_gridded_nimble_results", i, 
                                              ".rds"))
  
}

