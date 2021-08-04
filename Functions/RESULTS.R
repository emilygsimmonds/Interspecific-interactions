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

results_m <- summarise_data(pattern_string = "Baseline",
                          interaction_type = "m")

results_p <- summarise_data(pattern_string = "Baseline",
                            interaction_type = "p")

results_c <- summarise_data(pattern_string = "Baseline",
                            interaction_type = "c")

results_baseline_inla <- c(results_m[[1]], 
                             results_p[[1]], 
                             results_c[[1]])

results_baseline_nimble <- c(results_m[[2]], 
                      results_p[[2]], 
                      results_c[[2]])

################################################################################

#### calculate results columns ####

# for each list entry calculate if sign is correct, if sign is clear, and error

results_baseline_inla_editted <- map_df(.x = results_baseline_inla, ~{
  .x <- mutate(as.data.frame(.x), 
               sign_correct = sign(.x[,1]) == sign(.x$true),
               sign_clear = sign(.x[,2]) == sign(.x[,3]),
               error = .x[,1]-.x$true,
               label = rownames(as.data.frame(.x)))
})

results_baseline_nimble_editted <- map_df(.x = results_baseline_nimble, ~{
  .x <- mutate(as.data.frame(.x), 
               sign_correct = sign(.x[,1]) == sign(.x$true),
               sign_clear = sign(.x[,2]) == sign(.x[,3]),
               error = .x[,1]-.x$true,
               label = rownames(as.data.frame(.x)))
})



################################################################################

#### plot results ####

### % sign correct ###

inla_sign_correct <- results_baseline_inla_editted %>% 
  drop_na(sign_correct) %>%
  group_by(interaction_type, label, sign_correct) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count/sum(count))*100) %>%
  ungroup()

nimble_sign_correct <- results_baseline_nimble_editted %>% 
  drop_na(sign_correct) %>%
  group_by(interaction_type, label, sign_correct) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count/sum(count))*100) %>%
  ungroup()

ggplot(data = inla_sign_correct, 
             aes(x = sign_correct, y = percentage, fill = interaction_type))+
  geom_col() +
  facet_grid(rows = vars(label), cols = vars(interaction_type)) +
  labs(title = "Baseline results: INLA",
       ylab = "Percentage of results",
       xlab = "Sign correct?") +
  geom_rect(data = filter(inla_sign_correct, label == "alpha_ionj"|
                            label == "alpha_joni"),
            aes(fill = label),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf, alpha = 0.3) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(data = nimble_sign_correct, 
             aes(x = sign_correct, y = percentage, fill = interaction_type))+
  geom_col() +
  facet_grid(rows = vars(label), cols = vars(interaction_type)) +
  labs(title = "Baseline results: Naive Nimble",
       ylab = "Percentage of results",
       xlab = "Sign correct?") +
  geom_rect(data = filter(nimble_sign_correct, label == "alpha_ij"|
                            label == "alpha_ji"),
            aes(fill = label),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf, alpha = 0.3) +
  theme_minimal() +
  theme(legend.position = "none")


################################################################################
results_baseline_gridded <- c(results_m[[3]], 
                              results_p[[3]], 
                              results_c[[3]])

results_baseline_gridded_editted <- map_df(.x = results_baseline_gridded, ~{
  .x <- mutate(as.data.frame(.x), 
               sign_correct = sign(.x[,1]) == sign(.x$true),
               sign_clear = sign(.x[,2]) == sign(.x[,3]),
               error = .x[,1]-.x$true,
               label = rownames(as.data.frame(.x)))
})

gridded_sign_correct <- results_baseline_gridded_editted %>% 
  drop_na(sign_correct) %>%
  group_by(interaction_type, label, sign_correct) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count/sum(count))*100) %>%
  ungroup()

ggplot(data = gridded_sign_correct, 
       aes(x = sign_correct, y = percentage, fill = interaction_type))+
  geom_col() +
  facet_grid(rows = vars(label), cols = vars(interaction_type)) +
  labs(title = "Baseline results: Gridded Nimble ",
       ylab = "Percentage of results",
       xlab = "Sign correct?") +
  geom_rect(data = filter(gridded_sign_correct, label == "alpha_ij"|
                            label == "alpha_ji"),
            aes(fill = label),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf, alpha = 0.3) +
  theme_minimal() +
  theme(legend.position = "none")