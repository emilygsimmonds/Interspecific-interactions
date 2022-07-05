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

###############################################################################

#### IMPACT OF INTERACTIONS: Simulations based on fitted model parameters ####
#### BASELINE ####

# For each type of interaction, original simulation gave 500 datasets
# Repeat this, but using parameters estimated during model fitting
# Using all results will give WAY too many simulations
# Start with say the first 10 results from each model

#### Pull in the results ####

source('./Functions/RESULTS_baseline.R')

#### Prune results to a single interaction type and model ####

# need to split results to only a single model and interaction combo
# also need to make sure each entry of a list is a single model output
# do this by creating own grouping variable and then using group_split
test_dataset <- filter(Model == "Nimble",
                       interaction == "cc") %>%
  mutate(Group = rep(1:100, each = 7)) %>%
  group_by(Group) %>%
  group_split

#### Extract the parameter values and run simulation ####

Simulation_Nimble_CC <- Extract_parameters(test_dataset)