# Results plot

################################################################################

#### plot results ####

### raw estimates ###

raw <- results_baseline_all %>% 
  drop_na(error) %>%
  mutate(label = as.factor(label))

ggplot(data = filter(raw, label == "alpha_ionj" |
                       label == "alpha_joni"), 
       aes(y = Mean, x = interaction_type, fill = model))+
  geom_violin(stat = "ydensity", alpha = 0.5) +
  geom_hline(yintercept = 0)+
  facet_grid(cols = vars(interaction_type), rows = vars(label),
             scales = "free_x") +
  labs(title = "baseline results: mean",
       y = "estimate",
       x = "interaction type") +
  theme_minimal() 

ggsave(last_plot(), file = "./Figures/baseline_raw.png")

### % sign correct ###

sign_correct_b <- sign_correct <- results_baseline_all %>% 
  drop_na(sign_correct) %>%
  group_by(model, interaction_type, label, sign_correct) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count/sum(count))*100) %>%
  filter(sign_correct == TRUE) %>%
  mutate(sign_correct = droplevels(as.factor(sign_correct))) %>%
  ungroup()

ggplot(data = filter(sign_correct, 
                     label == "alpha_ionj" |
                       label == "alpha_joni"), 
       aes(x = interaction_type, y = percentage, fill = model))+
  geom_col(position = "dodge") +
  facet_wrap(vars(label),
             scales = "free_x") +
  labs(title = "Baseline Results",
       y = "% Sign Correct",
       x = "Interaction type")  +
  scale_fill_viridis_d(breaks = c("inla", "nimble"), 
                       labels = c("R-INLA", "Nimble")) +
  theme_minimal()

ggsave(last_plot(), file = "./Figures/baseline_sign_correct.png")


### % sign clear###

sign_clear <- results_baseline_all %>% 
  drop_na(sign_clear) %>%
  group_by(model, interaction_type, label, sign_clear) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count/sum(count))*100) %>%
  filter(sign_clear == TRUE) %>%
  mutate(sign_clear = droplevels(as.factor(sign_clear))) %>%
  ungroup()


ggplot(data = filter(sign_clear, 
                     label == "alpha_ionj" |
                       label == "alpha_joni" | 
                       label == "Rho"), 
       aes(x = interaction_type, y = percentage, fill = model))+
  geom_rect(data = filter(sign_clear, label == "alpha_ionj"|
                            label == "alpha_joni"),
            aes(fill = "grey"),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf, alpha = 0.3) +
  geom_col(position = "dodge") +
  facet_grid(rows = vars(label), cols = vars(interaction_type)) +
  labs(title = "baseline results: % sign clear",
       y = "Percentage of results",
       x = "Sign clear") +
  scale_fill_viridis_d(breaks = c("gridded", "inla", "nimble")) +
  theme_minimal() 

ggsave(last_plot(), file = "./Figures/baseline_sign_clear.png")


### error ###

error <- results_baseline_all %>% 
  drop_na(error) %>%
  mutate(label = as.factor(label))

ggplot(data = filter(error, label == "alpha_ionj" |
                       label == "alpha_joni"), 
       aes(y = error, x = interaction_type, fill = model))+
  geom_violin(stat = "ydensity", alpha = 0.5) +
  geom_hline(yintercept = 0)+
  facet_grid(cols = vars(interaction_type), rows = vars(label),
             scales = "free_x") +
  labs(title = "baseline results: error",
       ylab = "error",
       xlab = "interaction type") +
  theme_minimal() 

ggsave(last_plot(), file = "./Figures/baseline_error.png")


#############################

################################################################################

#### plot results ####

### raw estimates ###

raw <- results_corr1_all %>% 
  drop_na(error) %>%
  mutate(label = as.factor(label))

ggplot(data = filter(raw, label == "alpha_ionj" |
                       label == "alpha_joni"), 
       aes(y = Mean, x = interaction_type, fill = model))+
  geom_violin(stat = "ydensity", alpha = 0.5) +
  geom_hline(yintercept = 0)+
  facet_grid(cols = vars(interaction_type), rows = vars(label),
             scales = "free_x") +
  labs(title = "corr 1 results: mean",
       y = "estimate",
       x = "interaction type") +
  theme_minimal() 

ggsave(last_plot(), file = "./Figures/corr1_raw.png")


### % sign correct ###

sign_correct <- results_corr1_all %>% 
  drop_na(sign_correct) %>%
  group_by(model, interaction_type, label, sign_correct) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count/sum(count))*100) %>%
  filter(sign_correct == TRUE) %>%
  mutate(sign_correct = droplevels(as.factor(sign_correct))) %>%
  ungroup()

sign_correct$change <- sign_correct$percentage - sign_correct_b$percentage

ggplot(data = filter(sign_correct, 
                     label == "alpha_ionj" |
                       label == "alpha_joni"), 
       aes(x = interaction_type, y = change, fill = model))+
  geom_col(position = "dodge") +
  facet_wrap(vars(label),#rows = vars(label), cols = vars(interaction_type),
             scales = "free_x") +
  labs(title = "Rho = 0.9 Results",
       y = "Change From Unequal Scenario",
       x = "Interaction Type")  +
  scale_fill_viridis_d(breaks = c("inla", "nimble"),
                       labels =c("R-INLA", "Nimble")) +
  theme_minimal()

ggsave(last_plot(), file = "./Figures/corr1_sign_correct.png")


### % sign clear###

sign_clear <- results_corr1_all %>% 
  drop_na(sign_clear) %>%
  group_by(model, interaction_type, label, sign_clear) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count/sum(count))*100) %>%
  filter(sign_clear == TRUE) %>%
  mutate(sign_clear = droplevels(as.factor(sign_clear))) %>%
  ungroup()


ggplot(data = filter(sign_clear, 
                     label == "alpha_ionj" |
                       label == "alpha_joni" | 
                       label == "Rho"), 
       aes(x = interaction_type, y = percentage, fill = model))+
  geom_rect(data = filter(sign_clear, label == "alpha_ionj"|
                            label == "alpha_joni"),
            aes(fill = "grey"),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf, alpha = 0.3) +
  geom_col(position = "dodge") +
  facet_grid(rows = vars(label), cols = vars(interaction_type)) +
  labs(title = "corr1 results: % sign clear",
       y = "Percentage of results",
       x = "Sign clear") +
  scale_fill_viridis_d(breaks = c("gridded", "inla", "nimble")) +
  theme_minimal() 

ggsave(last_plot(), file = "./Figures/corr1_sign_clear.png")


### error ###

error <- results_corr1_all %>% 
  drop_na(error) %>%
  mutate(label = as.factor(label))

ggplot(data = filter(error, label == "alpha_ionj" |
                       label == "alpha_joni"), 
       aes(y = error, x = interaction_type, fill = model))+
  geom_violin(stat = "ydensity") +
  geom_hline(yintercept = 0)+
  facet_grid(cols = vars(interaction_type), rows = vars(label),
             scales = "free_x") +
  labs(title = "corr1 results: error",
       y = "error",
       x = "Parameter") +
  theme_minimal() 

ggsave(last_plot(), file = "./Figures/corr1_error.png")


#############################

################################################################################

#### plot results ####

### raw estimates ###

raw <- results_corr2_all %>% 
  drop_na(error) %>%
  mutate(label = as.factor(label))

ggplot(data = filter(raw, label == "alpha_ionj" |
                       label == "alpha_joni"), 
       aes(y = Mean, x = interaction_type, fill = model))+
  geom_violin(stat = "ydensity", alpha = 0.5) +
  geom_hline(yintercept = 0)+
  facet_grid(cols = vars(interaction_type), rows = vars(label),
             scales = "free_x") +
  labs(title = "corr 2 results: mean",
       y = "estimate",
       x = "interaction type") +
  theme_minimal() 

ggsave(last_plot(), file = "./Figures/corr2_raw.png")

### % sign correct ###

sign_correct <- results_corr2_all %>% 
  drop_na(sign_correct) %>%
  group_by(model, interaction_type, label, sign_correct) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count/sum(count))*100) %>%
  filter(sign_correct == TRUE) %>%
  mutate(sign_correct = droplevels(as.factor(sign_correct))) %>%
  ungroup()

sign_correct$change <- sign_correct$percentage - sign_correct_b$percentage

ggplot(data = filter(sign_correct, 
                     label == "alpha_ionj" |
                       label == "alpha_joni"), 
       aes(x = interaction_type, y = change, fill = model))+
  geom_col(position = "dodge") +
  facet_wrap(vars(label),#rows = vars(label), cols = vars(interaction_type),
             scales = "free_x") +
  labs(title = "Rho = 0.5 Results",
       y = "Change From Unequal Scenario",
       x = "Interaction Type")  +
  scale_fill_viridis_d(breaks = c("inla", "nimble"),
                       labels =c("R-INLA", "Nimble")) +
  theme_minimal()

ggsave(last_plot(), file = "./Figures/corr2_sign_correct.png")


### % sign clear###

sign_clear <- results_corr2_all %>% 
  drop_na(sign_clear) %>%
  group_by(model, interaction_type, label, sign_clear) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count/sum(count))*100) %>%
  filter(sign_clear == TRUE) %>%
  mutate(sign_clear = droplevels(as.factor(sign_clear))) %>%
  ungroup()


ggplot(data = filter(sign_clear, 
                     label == "alpha_ionj" |
                       label == "alpha_joni" | 
                       label == "Rho"), 
       aes(x = interaction_type, y = percentage, fill = model))+
  geom_rect(data = filter(sign_clear, label == "alpha_ionj"|
                            label == "alpha_joni"),
            aes(fill = "grey"),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf, alpha = 0.3) +
  geom_col(position = "dodge") +
  facet_grid(rows = vars(label), cols = vars(interaction_type)) +
  labs(title = "corr2 results: % sign clear",
       y = "Percentage of results",
       x = "Sign clear") +
  scale_fill_viridis_d(breaks = c("gridded", "inla", "nimble")) +
  theme_minimal() 

ggsave(last_plot(), file = "./Figures/corr2_sign_clear.png")


### error ###

error <- results_corr2_all %>% 
  drop_na(error) %>%
  mutate(label = as.factor(label))

ggplot(data = filter(error, label == "alpha_ionj" |
                       label == "alpha_joni"), 
       aes(y = error, x = interaction_type, fill = model))+
  geom_violin(stat = "ydensity") +
  geom_hline(yintercept = 0)+
  facet_grid(cols = vars(interaction_type), rows = vars(label),
             scales = "free_x") +
  labs(title = "corr2 results: error",
       y = "error",
       x = "Parameter") +
  theme_minimal() 

ggsave(last_plot(), file = "./Figures/corr2_error.png")


#############################

################################################################################

#### plot results ####

### raw estimates ###

raw <- results_corr3_all %>% 
  drop_na(error) %>%
  mutate(label = as.factor(label))

ggplot(data = filter(raw, label == "alpha_ionj" |
                       label == "alpha_joni"), 
       aes(y = Mean, x = interaction_type, fill = model))+
  geom_violin(stat = "ydensity", alpha = 0.5) +
  geom_hline(yintercept = 0)+
  facet_grid(cols = vars(interaction_type), rows = vars(label),
             scales = "free_x") +
  labs(title = "corr 3 results: mean",
       y = "estimate",
       x = "interaction type") +
  theme_minimal() 

ggsave(last_plot(), file = "./Figures/corr3_raw.png")

### % sign correct ###

sign_correct <- results_corr3_all %>% 
  drop_na(sign_correct) %>%
  group_by(model, interaction_type, label, sign_correct) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count/sum(count))*100) %>%
  filter(sign_correct == TRUE,
         label != "prec[1, 2]" &
           label != "prec[2, 1]") %>%
  mutate(sign_correct = droplevels(as.factor(sign_correct))) %>%
  ungroup()

sign_correct$change <- sign_correct$percentage - sign_correct_b$percentage

ggplot(data = filter(sign_correct, 
                     label == "alpha_ionj" |
                       label == "alpha_joni"), 
       aes(x = interaction_type, y = change, fill = model))+
  geom_col(position = "dodge") +
  facet_wrap(vars(label),#rows = vars(label), cols = vars(interaction_type),
             scales = "free_x") +
  labs(title = "Rho = 0.3 Results",
       y = "Change From Unequal Scenario",
       x = "Interaction Type")  +
  scale_fill_viridis_d(breaks = c("inla", "nimble"),
                       labels =c("R-INLA", "Nimble")) +
  theme_minimal()

ggsave(last_plot(), file = "./Figures/corr3_sign_correct.png")


### % sign clear###

sign_clear <- results_corr3_all %>% 
  drop_na(sign_clear) %>%
  group_by(model, interaction_type, label, sign_clear) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count/sum(count))*100) %>%
  filter(sign_clear == TRUE) %>%
  mutate(sign_clear = droplevels(as.factor(sign_clear))) %>%
  ungroup()


ggplot(data = filter(sign_clear, 
                     label == "alpha_ionj" |
                       label == "alpha_joni" | 
                       label == "Rho"), 
       aes(x = interaction_type, y = percentage, fill = model))+
  geom_rect(data = filter(sign_clear, label == "alpha_ionj"|
                            label == "alpha_joni"),
            aes(fill = "grey"),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf, alpha = 0.3) +
  geom_col(position = "dodge") +
  facet_grid(rows = vars(label), cols = vars(interaction_type)) +
  labs(title = "corr3 results: % sign clear",
       y = "Percentage of results",
       x = "Sign clear") +
  scale_fill_viridis_d(breaks = c("gridded", "inla", "nimble")) +
  theme_minimal() 

ggsave(last_plot(), file = "./Figures/corr3_sign_clear.png")


### error ###

error <- results_corr3_all %>% 
  drop_na(error) %>%
  mutate(label = as.factor(label))

ggplot(data = filter(error, label == "alpha_ionj" |
                       label == "alpha_joni"), 
       aes(y = error, x = interaction_type, fill = model))+
  geom_violin(stat = "ydensity") +
  geom_hline(yintercept = 0)+
  facet_grid(cols = vars(interaction_type), rows = vars(label),
             scales = "free_x") +
  labs(title = "corr3 results: error",
       y = "error",
       x = "Parameter") +
  theme_minimal() 

ggsave(last_plot(), file = "./Figures/corr3_error.png")


#############################
################################################################################

#### plot results ####

### raw estimates ###

raw <- results_noise1_all %>% 
  drop_na(error) %>%
  mutate(label = as.factor(label))

ggplot(data = filter(raw, label == "alpha_ionj" |
                       label == "alpha_joni"), 
       aes(y = Mean, x = interaction_type, fill = model))+
  geom_violin(stat = "ydensity", alpha = 0.5) +
  geom_hline(yintercept = 0)+
  facet_grid(cols = vars(interaction_type), rows = vars(label),
             scales = "free_x") +
  labs(title = "noise 1 results: mean",
       y = "estimate",
       x = "interaction type") +
  theme_minimal() 

ggsave(last_plot(), file = "./Figures/noise1_raw.png")

### % sign correct ###

sign_correct <- results_noise1_all %>% 
  drop_na(sign_correct) %>%
  group_by(model, interaction_type, label, sign_correct) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count/sum(count))*100) %>%
  filter(sign_correct == TRUE) %>%
  mutate(sign_correct = droplevels(as.factor(sign_correct))) %>%
  ungroup()

sign_correct$change <- sign_correct$percentage - sign_correct_b$percentage

ggplot(data = filter(sign_correct, 
                     label == "alpha_ionj" |
                       label == "alpha_joni"), 
       aes(x = interaction_type, y = change, fill = model))+
  geom_col(position = "dodge") +
  facet_wrap(vars(label),#rows = vars(label), cols = vars(interaction_type),
             scales = "free_x") +
  labs(title = "Increased Noise Results",
       y = "Change From Baseline",
       x = "Interaction Type")  +
  scale_fill_viridis_d(breaks = c("inla", "nimble"),
                       labels =c("R-INLA", "Nimble")) +
  theme_minimal()

ggsave(last_plot(), file = "./Figures/noise1_sign_correct.png")


### % sign clear###

sign_clear <- results_noise1_all %>% 
  drop_na(sign_clear) %>%
  group_by(model, interaction_type, label, sign_clear) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count/sum(count))*100) %>%
  filter(sign_clear == TRUE) %>%
  mutate(sign_clear = droplevels(as.factor(sign_clear))) %>%
  ungroup()


ggplot(data = filter(sign_clear, 
                     label == "alpha_ionj" |
                       label == "alpha_joni" | 
                       label == "Rho"), 
       aes(x = interaction_type, y = percentage, fill = model))+
  geom_rect(data = filter(sign_clear, label == "alpha_ionj"|
                            label == "alpha_joni"),
            aes(fill = "grey"),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf, alpha = 0.3) +
  geom_col(position = "dodge") +
  facet_grid(rows = vars(label), cols = vars(interaction_type)) +
  labs(title = "noise1 results: % sign clear",
       y = "Percentage of results",
       x = "Sign clear") +
  scale_fill_viridis_d(breaks = c("gridded", "inla", "nimble")) +
  theme_minimal() 

ggsave(last_plot(), file = "./Figures/noise1_sign_clear.png")


### error ###

error <- results_noise1_all %>% 
  drop_na(error) %>%
  mutate(label = as.factor(label))

ggplot(data = filter(error, label == "alpha_ionj" |
                       label == "alpha_joni"), 
       aes(y = error, x = interaction_type, fill = model))+
  geom_violin(stat = "ydensity") +
  geom_hline(yintercept = 0)+
  facet_grid(cols = vars(interaction_type), rows = vars(label),
             scales = "free_x") +
  labs(title = "noise1 results: error",
       y = "error",
       x = "Parameter") +
  theme_minimal() 

ggsave(last_plot(), file = "./Figures/noise1_error.png")


#############################

################################################################################

#### plot results ####

raw <- results_noise2_all %>% 
  drop_na(error) %>%
  mutate(label = as.factor(label))

ggplot(data = filter(raw, label == "alpha_ionj" |
                       label == "alpha_joni"), 
       aes(y = Mean, x = interaction_type, fill = model))+
  geom_violin(stat = "ydensity", alpha = 0.5) +
  geom_hline(yintercept = 0)+
  facet_grid(cols = vars(interaction_type), rows = vars(label),
             scales = "free_x") +
  labs(title = "noise 2 results: mean",
       y = "estimate",
       x = "interaction type") +
  theme_minimal() 

ggsave(last_plot(), file = "./Figures/noise2_raw.png")

### % sign correct ###

sign_correct <- results_noise2_all %>% 
  drop_na(sign_correct) %>%
  group_by(model, interaction_type, label, sign_correct) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count/sum(count))*100) %>%
  filter(sign_correct == TRUE) %>%
  mutate(sign_correct = droplevels(as.factor(sign_correct))) %>%
  ungroup()

sign_correct$change <- sign_correct$percentage - sign_correct_b$percentage

ggplot(data = filter(sign_correct, 
                     label == "alpha_ionj" |
                       label == "alpha_joni"), 
       aes(x = interaction_type, y = change, fill = model))+
  geom_col(position = "dodge") +
  facet_wrap(vars(label),#rows = vars(label), cols = vars(interaction_type),
             scales = "free_x") +
  labs(title = "Increased Noise Results",
       y = "Change From Unequal Scenario",
       x = "Interaction Type")  +
  scale_fill_viridis_d(breaks = c("inla", "nimble"),
                       labels =c("R-INLA", "Nimble")) +
  theme_minimal()


ggsave(last_plot(), file = "./Figures/noise2_sign_correct.png")


### % sign clear###

sign_clear <- results_noise2_all %>% 
  drop_na(sign_clear) %>%
  group_by(model, interaction_type, label, sign_clear) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count/sum(count))*100) %>%
  filter(sign_clear == TRUE) %>%
  mutate(sign_clear = droplevels(as.factor(sign_clear))) %>%
  ungroup()

ggplot(data = filter(sign_clear, 
                     label == "alpha_ionj" |
                       label == "alpha_joni"), 
       aes(x = interaction_type, y = percentage, fill = model))+
  geom_rect(data = filter(sign_clear, label == "alpha_ionj"|
                            label == "alpha_joni"),
            aes(fill = "grey"),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf, alpha = 0.3) +
  geom_col(position = "dodge") +
  facet_grid(rows = vars(label), cols = vars(interaction_type)) +
  labs(title = "noise2 results: % sign clear",
       y = "Percentage of results",
       x = "Sign clear") +
  scale_fill_viridis_d(breaks = c("gridded", "inla", "nimble")) +
  theme_minimal() 

ggsave(last_plot(), file = "./Figures/noise2_sign_clear.png")


### error ###

error <- results_noise2_all %>% 
  drop_na(error) %>%
  mutate(label = as.factor(label))

ggplot(data = filter(error, label == "alpha_ionj" |
                       label == "alpha_joni"), 
       aes(y = error, x = interaction_type, fill = model))+
  geom_violin(stat = "ydensity") +
  geom_hline(yintercept = 0)+
  facet_grid(cols = vars(interaction_type), rows = vars(label),
             scales = "free_x") +
  labs(title = "noise2 results: error",
       y = "error",
       x = "Parameter") +
  theme_minimal() 

ggsave(last_plot(), file = "./Figures/noise2_error.png")


#############################

################################################################################

#### plot results ####

raw <- results_unequal_all %>% 
  drop_na(error) %>%
  mutate(label = as.factor(label))

ggplot(data = filter(raw, label == "alpha_ionj" |
                       label == "alpha_joni"), 
       aes(y = Mean, x = interaction_type, fill = model))+
  geom_violin(stat = "ydensity", alpha = 0.5) +
  geom_hline(yintercept = 0)+
  facet_grid(cols = vars(interaction_type), rows = vars(label),
             scales = "free_x") +
  labs(title = "unequal results: mean",
       y = "estimate",
       x = "interaction type") +
  theme_minimal() 

ggsave(last_plot(), file = "./Figures/unequal_raw.png")

### % sign correct ###

sign_correct <- results_unequal_all %>% 
  drop_na(sign_correct) %>%
  group_by(model, interaction_type, label, sign_correct) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count/sum(count))*100) %>%
  filter(sign_correct == TRUE) %>%
  mutate(sign_correct = droplevels(as.factor(sign_correct))) %>%
  ungroup()

sign_correct$change <- sign_correct$percentage - sign_correct_b$percentage

ggplot(data = filter(sign_correct, 
                     label == "alpha_ionj" |
                       label == "alpha_joni"), 
       aes(x = interaction_type, y = change, fill = model))+
  geom_col(position = "dodge") +
  facet_wrap(vars(label),#rows = vars(label), cols = vars(interaction_type),
             scales = "free_x") +
  labs(title = "Unequal Interaction Strength Results",
       y = "Change From Baseline",
       x = "Interaction Type")  +
  scale_fill_viridis_d(breaks = c("inla", "nimble"),
                       labels =c("R-INLA", "Nimble")) +
  theme_minimal()


ggsave(last_plot(), file = "./Figures/unequal_sign_correct.png")


### % sign clear###

sign_clear <- results_unequal_all %>% 
  drop_na(sign_clear) %>%
  group_by(model, interaction_type, label, sign_clear) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count/sum(count))*100) %>%
  filter(sign_clear == TRUE) %>%
  mutate(sign_clear = droplevels(as.factor(sign_clear))) %>%
  ungroup()


ggplot(data = filter(sign_clear, 
                     label == "alpha_ionj" |
                       label == "alpha_joni" | 
                       label == "Rho"), 
       aes(x = interaction_type, y = percentage, fill = model))+
  geom_rect(data = filter(sign_clear, label == "alpha_ionj"|
                            label == "alpha_joni"),
            aes(fill = "grey"),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf, alpha = 0.3) +
  geom_col(position = "dodge") +
  facet_grid(rows = vars(label), cols = vars(interaction_type)) +
  labs(title = "unequal results: % sign clear",
       y = "Percentage of results",
       x = "Sign clear") +
  scale_fill_viridis_d(breaks = c("gridded", "inla", "nimble")) +
  theme_minimal() 

ggsave(last_plot(), file = "./Figures/unequal_sign_clear.png")


### error ###

error <- results_unequal_all %>% 
  drop_na(error) %>%
  mutate(label = as.factor(label))

ggplot(data = filter(error, label == "alpha_ionj" |
                       label == "alpha_joni"), 
       aes(y = error, x = interaction_type, fill = model))+
  geom_violin(stat = "ydensity") +
  geom_hline(yintercept = 0)+
  facet_grid(cols = vars(interaction_type), rows = vars(label),
             scales = "free_x") +
  labs(title = "unequal results: error",
       y = "error",
       x = "Parameter") +
  theme_minimal() 

ggsave(last_plot(), file = "./Figures/unequal_error.png")


################################################################################
