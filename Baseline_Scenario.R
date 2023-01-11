# Script to run baseline scenario

###############################################################################

#### source user defined functions ####

#source("./Functions/NimbleModelNaive_NI.R")
source("./Functions/INLAModel.R")
source("./Functions/runINLAModel.R")
#source("./Functions/runINLAModel_NI.R")

#### Load simulated time series ####

load("./Simulated data/simulated_TS_2021.RData")

#### run naive model ####

source("./Functions/NimbleModelNaive.R")

baseline_naive_nimble <- run_naive_nimble_model(simulations_all)

saveRDS(baseline_naive_nimble, file="Results/Baseline_naive_nimble_results_2022.rds")

source("./Functions/NimbleModelNaive_NI.R")

baseline_naive_nimble_NI <- run_naive_nimble_model(simulations_all)

saveRDS(baseline_naive_nimble_NI, file="Results/Baseline_naive_nimble_results_NI_2022.rds")

#### run inla model ####

baseline_inla <- run_inla_model_parallel(simulations_all)

saveRDS(baseline_inla, file="Results/Baseline_inla_results_2022.rds")

source("./Functions/runINLAModel_NI.R")

baseline_inla_NI <- run_inla_model_parallel(simulations_all)

saveRDS(baseline_inla_NI, file="Results/Baseline_inla_results_NI_2022.rds")
