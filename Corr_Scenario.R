# Script to run correlation scenarios

###############################################################################

#### source user defined functions ####

source("NimbleModelNaive.R")
source("NimbleModelGridded.R")
source("INLAModel.R")

#### Load simulated time series ####

load("simulated_TS_CORR3_2021.RData")
simulations_all3 <- simulations_all
load("simulated_TS_CORR2_2021.RData")
simulations_all2 <- simulations_all
load("simulated_TS_CORR1_2021.RData")

#### run naive model ####

corr1_naive_nimble <- run_naive_nimble_model(simulations_all)

saveRDS(corr1_naive_nimble, file="Results/corr1_naive_nimble_results.rds")

corr2_naive_nimble <- run_naive_nimble_model(simulations_all2)

saveRDS(corr2_naive_nimble, file="Results/corr2_naive_nimble_results.rds")

corr3_naive_nimble <- run_naive_nimble_model(simulations_all3)

saveRDS(corr3_naive_nimble, file="Results/corr3_naive_nimble_results.rds")

#### run gridded approach model ####

# size = number of different alphas to check 5 gives 25 total combos

for(i in seq(1, 300, 50)){
  
  corr1_gridded_nimble <- run_gridded_nimble_model(simulations_all[i:(i+49)], 
                                                    size = 5)
  saveRDS(corr1_gridded_nimble, 
          file=paste0("Results/corr1_gridded_nimble_results", i, 
                      ".rds"))
  
}

for(i in seq(1, 300, 50)){
  
  corr2_gridded_nimble <- run_gridded_nimble_model(simulations_all2[i:(i+49)], 
                                                    size = 5)
  saveRDS(corr2_gridded_nimble, 
          file=paste0("Results/corr2_gridded_nimble_results", i, 
                      ".rds"))
  
}

for(i in seq(1, 300, 50)){
  
  corr3_gridded_nimble <- run_gridded_nimble_model(simulations_all3[i:(i+49)], 
                                                   size = 5)
  saveRDS(corr3_gridded_nimble, 
          file=paste0("Results/corr3_gridded_nimble_results", i, 
                      ".rds"))
  
}

#### run inla model ####

corr1_inla <- run_inla_model_parallel(simulations_all)

saveRDS(corr1_inla, file="corr1_inla_results.rds")

corr2_inla <- run_inla_model_parallel(simulations_all2)

saveRDS(corr2_inla, file="corr2_inla_results.rds")

corr3_inla <- run_inla_model_parallel(simulations_all3)

saveRDS(corr3_inla, file="corr3_inla_results.rds")
