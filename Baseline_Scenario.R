# Script to run baseline scenario

###############################################################################

#### source user defined functions ####

source("NimbleModelNaive.R")
source("NimbleModelGridded.R")
source("INLAModel.R")
source("runINLAModel.R")

#### Load simulated time series ####

load("simulated_TS_2020.RData")

#### run naive model ####

baseline_naive_nimble <- run_naive_nimble_model(simulations_all)

saveRDS(baseline_naive_nimble, file="Baseline_naive_nimble_results.rds")

#### run gridded approach model ####

# size = number of different alphas to check 5 gives 25 total combos

for(i in seq(1, 300, 50)){
  
  baseline_gridded_nimble <- run_gridded_nimble_model(simulations_all[i:(i+49)], 
                                                      size = 5)
  saveRDS(baseline_gridded_nimble, 
       file=paste0("Results/Baseline_gridded_nimble_results", i, 
                                           ".rds"))
  
}

#### run inla model ####

baseline_inla <- run_inla_model_parallel(simulations_all)

#error_results <- lapply(baseline_inla, `[[`, "error")

saveRDS(baseline_inla, file="Baseline_inla_results.rds")