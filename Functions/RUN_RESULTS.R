################################################################################

#### Script to run all results

################################################################################

#### packages

library(tidyverse)
library(INLA)
library(nimble)
library(patchwork)
library(viridis)

#### scripts

source('./Functions/RESULTS_baseline.R')
source('./Functions/RESULTS_unequal.R')
source('./Functions/RESULTS_Noise1.R')
source('./Functions/RESULTS_Noise2.R')
source('./Functions/RESULTS_Corr1.R')
source('./Functions/RESULTS_Corr2.R')
source('./Functions/RESULTS_Corr3.R')

################################################################################

