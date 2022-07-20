# This script runs simulations to create time series data of species i and j
# it uses results from the fitted models to set the parameters

###############################################################################

# If you open in RStudio go to Edit > Folding > Collapse all

###############################################################################

# Set up

#### Load required packages ####

require(tidyverse)

# you will also need packages MASS and dplyr installed

#### Source user defined functions ####

# function to iterate the simulation
source("./Functions/Simulation_function.R")
# uses the Gompertz_function.R

#### Pull in original simulations ####
#load("./Simulated data/simulated_TS_2021.RData")
load("./Simulated data/simulated_TS_UNCONSTRAINED.RData")

# split by interaction type
simulations_cc <- bind_rows(simulations_all[1:100]) 
simulations_mm <- bind_rows(simulations_all[101:200]) 
simulations_pp <- bind_rows(simulations_all[201:300]) 

###############################################################################

#### IMPACT OF INTERACTIONS: Simulations based on fitted model parameters ####
#### BASELINE ####

# For each type of interaction, original simulation gave 500 datasets
# Repeat this, but using parameters estimated during model fitting
# Using all results will give WAY too many simulations
# Start with say the first 10 results from each model

#### Pull in the results ####

source('./Functions/RESULTS_baseline.R')

###############################################################################

#### COMPETITION ####

#### Prune results to a single interaction type and model ####

# need to split results to only a single model and interaction combo
# also need to make sure each entry of a list is a single model output
# do this by creating own grouping variable and then using group_split
nimble_dataset <- results_baseline_all %>% filter(model == "nimble",
                       interaction_type == "c") %>%
  mutate(Group = rep(1:100, each = 110)) %>% # there are 110 parameters est in nimble model
  group_by(Group) %>%
  group_split


inla_dataset <- results_baseline_all %>% filter(model == "inla",
                                                interaction_type == "c") %>%
  mutate(Group = rep(1:100, each = 9)) %>% # 9 parameters est in inla model
  group_by(Group) %>%
  group_split

#### Extract the parameter values and run simulation ####

source('./Functions/Extract_parameters.R')

# run simulations
simulation_nimble_cc <- unlist(map(.x = nimble_dataset, 
                            ~{Extract_parameters(.x,
                                                       n = 50,
                                                       starts = log(c(100,100)),
                                                       burnin = 50,
                                                       seed = NULL,
                                                       maxiter = 50000,
                                                       prevent_extinction = FALSE)
  }), recursive = FALSE)

# some inla results have NaN for parameter estimates
# therefore these will not run and give an error
# add 'safely' function to catch them - do not use unlist yet!!!
simulation_inla_cc <- map(.x = inla_dataset, 
                                 safely(~{Extract_parameters(.x,
                                                 n = 50,
                                                 starts = log(c(100,100)),
                                                 burnin = 50,
                                                 seed = NULL,
                                                 maxiter = 50000,
                                                 prevent_extinction = FALSE)
                            }))

# remove all error entries and remove all 'results' that are NULL
simulation_inla_cc <- simulation_inla_cc %>% map("result") %>% compact() 
# compact removes results that were NULL (8 in total here)
simulation_inla_cc <- unlist(simulation_inla_cc, recursive = FALSE)


# add original simulations to beginning of each dataset
nimble_cc_all <- simulation_nimble_cc %>% bind_rows() %>% bind_rows(simulations_cc) %>%
  mutate(Label = c(rep("original", 5000), rep("fitted", 500000))) %>%
  pivot_longer(cols = c(N_i, N_j, Y_i, Y_j), names_to = "Count")

inla_cc_all <- simulation_inla_cc %>% bind_rows() %>% bind_rows(simulations_cc) %>%
  mutate(Label = c(rep("original", 5000), rep("fitted", 9200*50))) %>%
  pivot_longer(cols = c(N_i, N_j, Y_i, Y_j), names_to = "Count")

# make summary table
summary_table_nimble_cc <- nimble_cc_all %>% group_by(Label, Count) %>% 
  summarise(min = min(value),
                  mean = mean(value),
                  median = median(value),
                  max = max(value))

summary_table_inla_cc <- inla_cc_all %>% # remove infinite values
  filter(!is.infinite(value)) %>% group_by(Label, Count) %>% 
  summarise(min = min(value, na.rm = TRUE),
            mean = mean(value, na.rm = TRUE),
            median = median(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE)) # there are some NAs in Y_i and Y_j

# make summary by run i.e. how many runs from fitted model have 0 or > 100000
summary_table_by_simulation_nimble_cc <- nimble_cc_all[20001:(length(nimble_cc_all$value)),]%>%
  mutate(Group = rep(1:10000, each = 200)) %>%
  group_by(Group) %>%
  summarise(min = min(value),
            mean = mean(value),
            median = median(value),
            max = max(value))

summary_table_by_simulation_inla_cc <- inla_cc_all[20001:(length(inla_cc_all$value)),]%>%
  mutate(Group = rep(1:9200, each = 200)) %>%
  group_by(Group) %>%
  summarise(min = min(value),
            mean = mean(value),
            median = median(value),
            max = max(value))

summary_table_by_simulation_original_cc <- inla_cc_all[1:20000,]%>%
  mutate(Group = rep(1:100, each = 200)) %>%
  group_by(Group) %>%
  summarise(min = min(value),
            mean = mean(value),
            median = median(value),
            max = max(value))

# how many simulations have min of 0? and max > 100000 combine two models here
summary_table_by_simulation_cc <- data.frame(Model = c(rep("inla", 1),
                                                       rep("nimble", 1),
                                                       rep("original", 1)),
                                             Percent_at_0 = NA,
                                             Percent_over_100000 = NA)

summary_table_by_simulation_cc[1,2] <- length(which(summary_table_by_simulation_inla_cc$min == 0))/
  length(summary_table_by_simulation_inla_cc$min)
summary_table_by_simulation_cc[1,3] <- length(which(summary_table_by_simulation_inla_cc$max > 100000))/
  length(summary_table_by_simulation_inla_cc$min)

summary_table_by_simulation_cc[2,2] <- length(which(summary_table_by_simulation_nimble_cc$min == 0))/
  length(summary_table_by_simulation_nimble_cc$min)
summary_table_by_simulation_cc[2,3] <- length(which(summary_table_by_simulation_nimble_cc$max > 100000))/
  length(summary_table_by_simulation_nimble_cc$min)

summary_table_by_simulation_cc[3,2] <- length(which(summary_table_by_simulation_original_cc$min == 0))/
  length(summary_table_by_simulation_original_cc$min)
summary_table_by_simulation_cc[3,3] <- length(which(summary_table_by_simulation_original_cc$max > 100000))/
  length(summary_table_by_simulation_original_cc$min)

# save out tables
write.csv(summary_table_nimble_cc, "summary_table_nimble_cc.csv")
write.csv(summary_table_inla_cc, "summary_table_inla_cc.csv")

# only want to save out % of times simulations exceeded 100000 and reached 0
write.csv(summary_table_by_simulation_cc, "summary_table_by_simulation_cc.csv")

###############################################################################

#### MUTUALISM ####

#### Prune results to a single interaction type and model ####

# need to split results to only a single model and interaction combo
# also need to make sure each entry of a list is a single model output
# do this by creating own grouping variable and then using group_split
nimble_dataset <- results_baseline_all %>% filter(model == "nimble",
                                                  interaction_type == "m") %>%
  mutate(Group = rep(1:100, each = 110)) %>% # there are 110 parameters est in nimble model
  group_by(Group) %>%
  group_split


inla_dataset <- results_baseline_all %>% filter(model == "inla",
                                                interaction_type == "m") %>%
  mutate(Group = rep(1:100, each = 9)) %>% # 9 parameters est in inla model
  group_by(Group) %>%
  group_split

#### Extract the parameter values and run simulation ####

source('./Functions/Extract_parameters.R')

# run simulations
simulation_nimble_mm <- unlist(map(.x = nimble_dataset, 
                                   ~{Extract_parameters(.x,
                                                        n = 50,
                                                        starts = log(c(100,100)),
                                                        burnin = 50,
                                                        seed = NULL,
                                                        maxiter = 50000,
                                                        prevent_extinction = FALSE)
                                   }), recursive = FALSE)

# some inla results have NaN for parameter estimates
# therefore these will not run and give an error
# add 'safely' function to catch them - do not use unlist yet!!!
simulation_inla_mm <- map(.x = inla_dataset, 
                          safely(~{Extract_parameters(.x,
                                                      n = 50,
                                                      starts = log(c(100,100)),
                                                      burnin = 50,
                                                      seed = NULL,
                                                      maxiter = 50000,
                                                      prevent_extinction = FALSE)
                          }))

# remove all error entries and remove all 'results' that are NULL
simulation_inla_mm <- simulation_inla_mm %>% map("result") %>% compact() 
# compact removes results that were NULL (1 in total here)
simulation_inla_mm <- unlist(simulation_inla_mm, recursive = FALSE)


# add original simulations to beginning of each dataset
nimble_mm_all <- simulation_nimble_mm %>% bind_rows() %>% bind_rows(simulations_mm) %>%
  mutate(Label = c(rep("original", 5000), rep("fitted", 500000))) %>%
  pivot_longer(cols = c(N_i, N_j, Y_i, Y_j), names_to = "Count")

inla_mm_all <- simulation_inla_mm %>% bind_rows() %>% bind_rows(simulations_mm) %>%
  mutate(Label = c(rep("original", 5000), rep("fitted", 9900*50))) %>%
  pivot_longer(cols = c(N_i, N_j, Y_i, Y_j), names_to = "Count")

# make summary table
summary_table_nimble_mm <- nimble_mm_all %>% group_by(Label, Count) %>% 
  summarise(min = min(value),
            mean = mean(value),
            median = median(value),
            max = max(value))

summary_table_inla_mm <- inla_mm_all %>% # remove infinite values and select only fitted
  filter(!is.infinite(value), Label == "fitted") %>% 
  group_by(Count) %>% 
  summarise(min = min(value, na.rm = TRUE),
            mean = mean(value, na.rm = TRUE),
            median = median(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE)) # there are some NAs in Y_i and Y_j

# make summary by run i.e. how many runs from fitted model have 0 or > 100000
summary_table_by_simulation_nimble_mm <- nimble_mm_all[20001:(length(nimble_mm_all$value)),]%>%
  mutate(Group = rep(1:10000, each = 200)) %>%
  group_by(Group) %>%
  summarise(min = min(value),
            mean = mean(value),
            median = median(value),
            max = max(value))

summary_table_by_simulation_inla_mm <- inla_mm_all[20001:(length(inla_mm_all$value)),]%>%
  mutate(Group = rep(1:9900, each = 200)) %>%
  group_by(Group) %>%
  summarise(min = min(value),
            mean = mean(value),
            median = median(value),
            max = max(value))

summary_table_by_simulation_original_mm <- inla_mm_all[1:20000,]%>%
  mutate(Group = rep(1:100, each = 200)) %>%
  group_by(Group) %>%
  summarise(min = min(value),
            mean = mean(value),
            median = median(value),
            max = max(value))

# how many simulations have min of 0? and max > 100000 combine two models here
summary_table_by_simulation_mm <- data.frame(Model = c(rep("inla", 1),
                                                       rep("nimble", 1),
                                                       rep("original", 1)),
                                             Percent_at_0 = NA,
                                             Percent_over_100000 = NA)

summary_table_by_simulation_mm[1,2] <- length(which(summary_table_by_simulation_inla_mm$min == 0))/
  length(summary_table_by_simulation_inla_mm$min)
summary_table_by_simulation_mm[1,3] <- length(which(summary_table_by_simulation_inla_mm$max > 100000))/
  length(summary_table_by_simulation_inla_mm$min)

summary_table_by_simulation_mm[2,2] <- length(which(summary_table_by_simulation_nimble_mm$min == 0))/
  length(summary_table_by_simulation_nimble_mm$min)
summary_table_by_simulation_mm[2,3] <- length(which(summary_table_by_simulation_nimble_mm$max > 100000))/
  length(summary_table_by_simulation_nimble_mm$min)

summary_table_by_simulation_mm[3,2] <- length(which(summary_table_by_simulation_original_mm$min == 0))/
  length(summary_table_by_simulation_original_mm$min)
summary_table_by_simulation_mm[3,3] <- length(which(summary_table_by_simulation_original_mm$max > 100000))/
  length(summary_table_by_simulation_original_mm$min)

# save out tables
write.csv(summary_table_nimble_mm, "summary_table_nimble_mm.csv")
write.csv(summary_table_inla_mm, "summary_table_inla_mm.csv")

# only want to save out % of times simulations exceeded 100000 and reached 0
write.csv(summary_table_by_simulation_mm, "summary_table_by_simulation_mm.csv")


###############################################################################

#### PREDATOR PREY ####


#### Prune results to a single interaction type and model ####

# need to split results to only a single model and interaction combo
# also need to make sure each entry of a list is a single model output
# do this by creating own grouping variable and then using group_split
nimble_dataset <- results_baseline_all %>% filter(model == "nimble",
                                                  interaction_type == "p") %>%
  mutate(Group = rep(1:100, each = 110)) %>% # there are 110 parameters est in nimble model
  group_by(Group) %>%
  group_split


inla_dataset <- results_baseline_all %>% filter(model == "inla",
                                                interaction_type == "p") %>%
  mutate(Group = rep(1:99, each = 9)) %>% # 9 parameters est in inla model
  # seem to only have 99 results here
  group_by(Group) %>%
  group_split

#### Extract the parameter values and run simulation ####

source('./Functions/Extract_parameters.R')

# run simulations
simulation_nimble_pp <- unlist(map(.x = nimble_dataset, 
                                   ~{Extract_parameters(.x,
                                                        n = 50,
                                                        starts = log(c(100,100)),
                                                        burnin = 50,
                                                        seed = NULL,
                                                        maxiter = 50000,
                                                        prevent_extinction = FALSE)
                                   }), recursive = FALSE)

# some inla results have NaN for parameter estimates
# therefore these will not run and give an error
# add 'safely' function to catch them - do not use unlist yet!!!
simulation_inla_pp <- map(.x = inla_dataset, 
                          safely(~{Extract_parameters(.x,
                                                      n = 50,
                                                      starts = log(c(100,100)),
                                                      burnin = 50,
                                                      seed = NULL,
                                                      maxiter = 50000,
                                                      prevent_extinction = FALSE)
                          }))

# remove all error entries and remove all 'results' that are NULL
simulation_inla_pp <- simulation_inla_pp %>% map("result") %>% compact() 
# compact removes results that were NULL (15 in total here)
simulation_inla_pp <- unlist(simulation_inla_pp, recursive = FALSE)


# add original simulations to beginning of each dataset
nimble_pp_all <- simulation_nimble_pp %>% bind_rows() %>% bind_rows(simulations_pp) %>%
  mutate(Label = c(rep("original", 5000), rep("fitted", 500000))) %>%
  pivot_longer(cols = c(N_i, N_j, Y_i, Y_j), names_to = "Count")

inla_pp_all <- simulation_inla_pp %>% bind_rows() %>% bind_rows(simulations_pp) %>%
  mutate(Label = c(rep("original", 5000), rep("fitted", 8400*50))) %>%
  pivot_longer(cols = c(N_i, N_j, Y_i, Y_j), names_to = "Count")

# make summary table
summary_table_nimble_pp <- nimble_pp_all %>% group_by(Label, Count) %>% 
  summarise(min = min(value),
            mean = mean(value),
            median = median(value),
            max = max(value))

summary_table_inla_pp <- inla_pp_all %>% # remove infinite values and select only fitted
  filter(!is.infinite(value), Label == "fitted") %>% 
  group_by(Count) %>% 
  summarise(min = min(value, na.rm = TRUE),
            mean = mean(value, na.rm = TRUE),
            median = median(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE)) # there are some NAs in Y_i and Y_j

# make summary by run i.e. how many runs from fitted model have 0 or > 100000
summary_table_by_simulation_nimble_pp <- nimble_pp_all[20001:(length(nimble_pp_all$value)),]%>%
  mutate(Group = rep(1:10000, each = 200)) %>%
  group_by(Group) %>%
  summarise(min = min(value),
            mean = mean(value),
            median = median(value),
            max = max(value))

summary_table_by_simulation_inla_pp <- inla_pp_all[20001:(length(inla_pp_all$value)),]%>%
  mutate(Group = rep(1:8400, each = 200)) %>%
  group_by(Group) %>%
  summarise(min = min(value),
            mean = mean(value),
            median = median(value),
            max = max(value))

summary_table_by_simulation_original_pp <- inla_pp_all[1:20000,]%>%
  mutate(Group = rep(1:100, each = 200)) %>%
  group_by(Group) %>%
  summarise(min = min(value),
            mean = mean(value),
            median = median(value),
            max = max(value))

# how many simulations have min of 0? and max > 100000 combine two models here
summary_table_by_simulation_pp <- data.frame(Model = c(rep("inla", 1),
                                                       rep("nimble", 1),
                                                       rep("original", 1)),
                                             Percent_at_0 = NA,
                                             Percent_over_100000 = NA)

summary_table_by_simulation_pp[1,2] <- length(which(summary_table_by_simulation_inla_pp$min == 0))/
  length(summary_table_by_simulation_inla_pp$min)
summary_table_by_simulation_pp[1,3] <- length(which(summary_table_by_simulation_inla_pp$max > 100000))/
  length(summary_table_by_simulation_inla_pp$min)

summary_table_by_simulation_pp[2,2] <- length(which(summary_table_by_simulation_nimble_pp$min == 0))/
  length(summary_table_by_simulation_nimble_pp$min)
summary_table_by_simulation_pp[2,3] <- length(which(summary_table_by_simulation_nimble_pp$max > 100000))/
  length(summary_table_by_simulation_nimble_pp$min)

summary_table_by_simulation_pp[3,2] <- length(which(summary_table_by_simulation_original_pp$min == 0))/
  length(summary_table_by_simulation_original_pp$min)
summary_table_by_simulation_pp[3,3] <- length(which(summary_table_by_simulation_original_pp$max > 100000))/
  length(summary_table_by_simulation_original_pp$min)

# save out tables
write.csv(summary_table_nimble_pp, "summary_table_nimble_pp.csv")
write.csv(summary_table_inla_pp, "summary_table_inla_pp.csv")

# only want to save out % of times simulations exceeded 100000 and reached 0
write.csv(summary_table_by_simulation_pp, "summary_table_by_simulation_pp.csv")
