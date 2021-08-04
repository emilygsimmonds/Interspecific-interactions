# Plots of results for importance of inter/intra specific interactions

################################################################################

load("simulated_TS_NO_INTERACTIONS.RData")
no_interaction <- simulations_all

load("simulated_TS_NO_INTra.RData")
no_intraspecific <- simulations_all

library(patchwork)
library(tidyverse)

################################################################################

### Set up dataframes

# Extract all Ni and Nj into a single dataframe
i <- as.list(1:400)

no_interaction_all <- map2_df(.x = no_interaction,
                              .y = i, ~{
  .x$label <- .y
  .x <- .x %>% pivot_longer(cols = c("N_i", "N_j"), 
                            names_to = "Type", 
                            values_to = "Counts")
  return(.x)
})

i <- as.list(1:300)

no_intra_all <- map2_df(.x = no_intraspecific,
                              .y = i, ~{
                              .x$label <- .y
                              .x <- .x %>% pivot_longer(cols = c("N_i", "N_j"), 
                                                        names_to = "Type", 
                                                        values_to = "Counts")
                              return(.x)
                              })

################################################################################

### Plots

# interspecific

no_interaction <- ggplot(data = filter(no_interaction_all, 
                                       label > 300),
                         aes(Counts,colour = Type))+
  geom_histogram(binwidth = 5)+
  xlim(-10, 1000)+
  ylim(0,4000)+
  facet_wrap(~Type)+
  labs(title = "No interaction",
       y = "frequency",
       x = "simulated population size")+
  theme_minimal()
  
no_interaction_c <- ggplot(data = filter(no_interaction_all, 
                                         label < 101),
                         aes(Counts,colour = Type))+
  geom_histogram(binwidth = 5)+
  xlim(-10, 1000)+
  ylim(0,4000)+
  facet_wrap(~Type)+
  labs(title = "Competition",
       y = "frequency",
       x = "simulated population size")+
theme_minimal()

no_interaction_m <- ggplot(data = filter(no_interaction_all, 
                                         label > 100 &
                                           label < 201),
                           aes(Counts,colour = Type))+
  geom_histogram(binwidth = 5)+
  xlim(-10, 30000)+
  ylim(0,4000)+
  facet_wrap(~Type)+
  labs(title = "Mutualism",
       y = "frequency",
       x = "simulated population size")+
theme_minimal()

no_interaction_p <- ggplot(data = filter(no_interaction_all, 
                                         label > 200 &
                                           label < 301),
                           aes(Counts,colour = Type))+
  geom_histogram(binwidth = 5)+
  xlim(-10, 1000)+
  ylim(0,4000)+
  facet_wrap(~Type)+
  labs(title = "Predator prey",
       y = "frequency",
       x = "simulated population size")+
theme_minimal()


no_interaction + no_interaction_m + no_interaction_c + no_interaction_p +
  plot_layout(nrow = 4)

ggsave(last_plot(), file = "no_interaction.png")


# intraspecific

no_intra_c <- ggplot(data = filter(no_intra_all, 
                                       label < 101),
                         aes(Counts,colour = Type))+
  geom_histogram(binwidth = 5)+
  xlim(-10, 1000)+
  ylim(0,4000)+
  facet_wrap(~Type)+
  labs(title = "No intraspecific: competition",
       y = "frequency",
       x = "simulated population size")+
  theme_minimal()

no_intra_m <- ggplot(data = filter(no_intra_all, 
                                         label > 100 &
                                     label < 201),
                           aes(Counts,colour = Type))+
  geom_histogram(binwidth = 5)+
  xlim(-10, 1000)+
  ylim(0,4000)+
  facet_wrap(~Type)+
  labs(title = "No intraspecific: mutulaism",
       y = "frequency",
       x = "simulated population size")+
  theme_minimal()

no_intra_p <- ggplot(data = filter(no_intra_all, 
                                         label > 200 &
                                           label < 301),
                           aes(Counts,colour = Type))+
  geom_histogram(binwidth = 5)+
  xlim(-10, 1000)+
  ylim(0,4000)+
  facet_wrap(~Type)+
  labs(title = "No intraspecific: predator prey",
       y = "frequency",
       x = "simulated population size")+
  theme_minimal()

no_interaction_m + no_intra_m +
  no_interaction_c + no_intra_c +
  no_interaction_p + no_intra_p +
  plot_layout(nrow = 3)

ggsave(last_plot(), file = "no_intra.png")

################################################################################

### summary table

N_i_summary <- bind_rows(summary(no_interaction_all$Counts[seq(30001,(30001+200*50), 2)]),
summary(no_interaction_all$Counts[seq(1,(200*50),2)]),
summary(no_interaction_all$Counts[seq(10001,(10001+200*50), 2)]),
summary(no_interaction_all$Counts[seq(20001,(20001+200*50), 2)]))

N_i_summary$Label <- c("no interaction",
                           "competition", 
                           "mutualism",
                           "predator prey")

N_j_summary <- bind_rows(summary(no_interaction_all$Counts[seq(30002,(30001+200*50), 2)]),
                         summary(no_interaction_all$Counts[seq(2,(200*50),2)]),
                         summary(no_interaction_all$Counts[seq(10002,(10001+200*50), 2)]),
                         summary(no_interaction_all$Counts[seq(20002,(20001+200*50), 2)]))

N_j_summary$Label <- c("no interaction",
                       "competition", 
                       "mutualism",
                       "predator prey")

write.csv(N_i_summary, "summary_Ni_nointeraction.csv")
write.csv(N_j_summary, "summary_Nj_nointeraction.csv")


N_i_summary <- bind_rows(
                         summary(no_intra_all$Counts[seq(1,(200*50),2)]),
                         summary(no_intra_all$Counts[seq(10001,(10001+200*50), 2)]),
                         summary(no_intra_all$Counts[seq(20001,(20001+200*50), 2)]))

N_i_summary$Label <- c(
                       "competition", 
                       "mutualism",
                       "predator prey")

N_j_summary <- bind_rows(
                         summary(no_intra_all$Counts[seq(2,(200*50),2)]),
                         summary(no_intra_all$Counts[seq(10002,(10001+200*50), 2)]),
                         summary(no_intra_all$Counts[seq(20002,(20001+200*50), 2)]))

N_j_summary$Label <- c(
                       "competition", 
                       "mutualism",
                       "predator prey")

write.csv(N_i_summary, "summary_Ni_nointra.csv")
write.csv(N_j_summary, "summary_Nj_nointra.csv")
