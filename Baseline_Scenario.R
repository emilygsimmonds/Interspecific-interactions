# Script to run baseline scenario

###############################################################################

#### source user defined functions ####

source("NimbleModelNaive.R")
#source("NimbleModelGridded.R")
source("INLAModel.R")
source("runINLAModel.R")

#### Load simulated time series ####

load("simulated_TS_2021.RData")

#### run naive model ####

baseline_naive_nimble <- run_naive_nimble_model(simulations_all)

saveRDS(baseline_naive_nimble, file="Results/Baseline_naive_nimble_results_2021.rds")

#### run inla model ####

baseline_inla <- run_inla_model_parallel(simulations_all)

#error_results <- lapply(baseline_inla, `[[`, "error")

saveRDS(baseline_inla, file="Results/Baseline_inla_results_2021.rds")