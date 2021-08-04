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

source('RESULTS_baseline.R')
source('RESULTS_unequal.R')
source('RESULTS_Noise1.R')
source('RESULTS_Noise2.R')
source('RESULTS_Corr1.R')
source('RESULTS_Corr2.R')
source('RESULTS_Corr3.R')

################################################################################

