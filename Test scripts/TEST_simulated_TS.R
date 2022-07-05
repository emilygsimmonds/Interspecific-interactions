# Script to plot simulated time series

###############################################################################

#### Load packages ####

library("tidyverse")

###############################################################################

#### Load simulated time series ####

load("simulated_TS_2021.RData")

names(simulations_all) <- 1:300

i <- as.list(1:300)

baseline_simulations <- map2_dfr(.x = simulations_all,
                               .y = i, ~{
  .x$Scenario <- .y 
  .x$Year <- 1:50
  if(.y < 101){.x$Interaction <- "cc"}
  if(.y > 100 & .y < 201){.x$Interaction <- "mm"}
  if(.y > 200){.x$Interaction <- "pp"}
  return(.x)
})


###############################################################################

summary(baseline_simulations)

#### Load simulated time series ####

load("simulated_TS_CORR3_2021.2.RData")

names(simulations_all) <- 1:300

i <- as.list(1:300)

baseline_simulations <- map2_dfr(.x = simulations_all,
                                 .y = i, ~{
                                   .x$Scenario <- .y 
                                   .x$Year <- 1:50
                                   if(.y < 101){.x$Interaction <- "cc"}
                                   if(.y > 100 & .y < 201){.x$Interaction <- "mm"}
                                   if(.y > 200){.x$Interaction <- "pp"}
                                   return(.x)
                                 })


###############################################################################

summary(baseline_simulations)


#### Plot them ####

### Baseline

ggplot(data = filter(baseline_simulations, Interaction != "mm"),
                   aes(x = Year,
                       y = N_j, 
                       colour = Scenario))+
  geom_line()+
  facet_wrap(~Interaction)
