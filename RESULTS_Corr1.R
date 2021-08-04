################################################################################

#### Function to import data and join for each scenario
# then create a summary to use for further analyses

################################################################################

#### packages

library(tidyverse)
library(INLA)
library(nimble)
library(patchwork)

#### scripts

source('./Functions/summarise_data.R')

################################################################################

#### make the summary data ####

results_m <- summarise_data(pattern_string = "corr1",
                            interaction_type = "m")

results_p <- summarise_data(pattern_string = "corr1",
                            interaction_type = "p")

results_c <- summarise_data(pattern_string = "corr1",
                            interaction_type = "c")

results_corr1_inla <- c(results_m[[1]], 
                         results_p[[1]], 
                         results_c[[1]])

results_corr1_nimble <- c(results_m[[2]], 
                           results_p[[2]], 
                           results_c[[2]])

#results_corr1_gridded <- c(results_m[[3]], 
#                            results_p[[3]], 
#                            results_c[[3]])

################################################################################

#### calculate results columns ####

# for each list entry calculate if sign is correct, if sign is clear, and error

results_corr1_inla_editted <- map_df(.x = results_corr1_inla, ~{
  .x <- mutate(as.data.frame(.x), 
               sign_correct = sign(.x[,1]) == sign(.x$true),
               sign_clear = sign(.x[,2]) == sign(.x[,3]),
               error = .x[,1]-.x$true,
               label = rownames(as.data.frame(.x)))
})

results_corr1_nimble_editted <- map_df(.x = results_corr1_nimble, ~{
  .x <- mutate(as.data.frame(.x), 
               sign_correct = sign(.x[,1]) == sign(.x$true),
               sign_clear = sign(.x[,2]) == sign(.x[,3]),
               error = .x[,1]-.x$true,
               label = rownames(as.data.frame(.x)))
})

#results_corr1_gridded_editted <- map_df(.x = results_corr1_gridded, ~{
#  .x <- mutate(as.data.frame(.x), 
#               sign_correct = sign(.x[,1]) == sign(.x$true),
#               sign_clear = sign(.x[,2]) == sign(.x[,3]),
#               error = .x[,1]-.x$true,
#               label = rownames(as.data.frame(.x)))
#})

results_corr1_all <- rbind(results_corr1_inla_editted,
                             results_corr1_nimble_editted)


