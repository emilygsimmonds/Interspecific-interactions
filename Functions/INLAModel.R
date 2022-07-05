# This script runs an INLA model in parallel

###############################################################################

# If you open in RStudio go to Edit > Folding > Collapse all

###############################################################################

run_inla_model_parallel <- function(simulations_all){

# Set up

#### Load required packages ####

library(INLA) # require for INLA
library(tictoc) # times code chunks
library(tidyverse) # for data manipulation and plotting
library(purrr) # allows parallel running

###############################################################################

#### INLA model ####

# restricted to first simulated scenario to keep run time short
  
#plan(multisession, workers = 2)  

tic() # starts a timer

ScenarioINLA <- map(simulations_all,
                    safely(run_inla_model))

toc() # print time taken

return(ScenarioINLA)

}
