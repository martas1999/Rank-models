
# libraries needed
library(knitr)
library(tidyverse)
library(forcats)

##  Loading the results grid grid 
grid_results_def_final <- readRDS("~/Desktop/thesis/rank_models/grid_results_def_final.rds")
nrow(grid_results_def_final) # 159000

# grid containing all the possible combinations of parameters
original_grid<- expand.grid(n_games = c(50, 250, 500, 1250),
            n_comp = c(15, 50, 250, 500),
            n_comp_per_game = c(2, 8, 20, 40),
            sigma = c(0.1, 1, 10))
nrow(original_grid) # 192 different combination
# percentage of conditions that were excluded a priori due to their unfeasibility 
159/192
1-0.828125

## NA Analysis  ------------------------------------------------------------------------------------------------


# Proportion of data simulations with  error
prop.table(table(grid_results_def_final$sim_data_error)) # 14% of the simulations of data failed, 22574 not av 
#159000-22574 = 136426 without sim error 

table(is.na(grid_results_def_final$beta0_mse)) # 22576, 2 (2/136426) near to 0% with no convergence detected
table(is.na(grid_results_def_final$rol_mse)) #  22574, no cases of convergence undetected 
table(is.na(grid_results_def_final$beta1_mse)) # 40539, 17965 , 13% with no convergence detected
table(is.na(grid_results_def_final$beta2_mse)) # 38571, 15997 , 12% with no convergence detected

# predictors of sim_data_error using lm()
sim_error_lm <- grid_results_def_final %>% 
  mutate(sim_data_error = ifelse(sim_data_error == TRUE, 1, 0))
summary(lm(sim_data_error ~ n_games + n_comp + n_comp_per_game + sigma, data = sim_error_lm)) #sigma is the only non-significant predictors
  
# number of failures to simulate the data for each  different combination of parameters
sim_failures <-grid_results_def_final %>% 
    filter(sim_data_error == TRUE) %>%
  group_by(n_games, n_comp, n_comp_per_game, sigma) %>% 
  summarise(
    count = n())
View(sim_failures)

##  NA for mse - Varying-Beta model 

# predictors of NA using lm() 
error_beta1_lm <- grid_results_def_final %>% 
  mutate(beta1_error = ifelse(is.na(beta1_mse), 1, 0))
summary(lm(beta1_error ~ n_games + n_comp + n_comp_per_game + sigma, data = error_beta1_lm)) #sigma is the only non-significant predictors


beta1_failures <- grid_results_def_final %>% 
  filter(sim_data_error == FALSE & is.na(beta1_mse)) %>% 
  mutate(beta1_mse = ifelse(is.na(beta1_mse), 1, 0)) %>%
  select(n_games, n_comp, n_comp_per_game, sigma, beta1_mse) %>%
  pivot_longer(
    cols = ends_with("mse"),
    names_to = "model",
    values_to = "mse",
    names_pattern = "(.*)_"
  ) %>% 
  group_by(n_games, n_comp, n_comp_per_game, sigma, model) %>% 
  summarise(
    count = n())
View(beta1_failures)
# 68 different combination

## -- NA for mse Dependent-Beta model

# predictors of NA using lm()
error_beta2_lm <- grid_results_def_final %>% 
  mutate(beta2_error = ifelse(is.na(beta2_mse), 1, 0))
summary(lm(beta2_error ~ n_games + n_comp + n_comp_per_game + sigma, data = error_beta2_lm)) #sigma is the only non-significant predictors

grid_results_def_final %>% 
  filter(sim_data_error == FALSE & is.na(beta2_mse) ) %>% 
  select(n_games, n_comp, n_comp_per_game, sigma, beta2_mse) %>%
  pivot_longer(
    cols = ends_with("mse"),
    names_to = "model",
    values_to = "mse",
    names_pattern = "(.*)_"
  ) %>% 
  group_by(n_games, n_comp, n_comp_per_game, sigma, model) %>% 
  summarise(
    count = n())
# 73 different combination

##  NA for Varying-Beta and Dependent-Beta together 
betas_failures <-grid_results_def_final %>% 
  filter(sim_data_error == FALSE) %>% 
  pivot_longer(
    cols = ends_with("mse"),
    names_to = "model",
    values_to = "mse",
    names_pattern = "(.*)_"
  ) %>% 
  filter(is.na(mse)) %>% 
  group_by(n_games, n_comp, n_comp_per_game, sigma,model) %>% 
  summarise(
    count = n())

View(betas_failures)
# 38 different combination

## tidy summary with average mse and number of NA in the estimation of the models
tidy_na_models <-grid_results_def_final %>% 
  filter(sim_data_error == FALSE) %>%
  pivot_longer(
    cols = ends_with("mse"),
    names_to = "model",
    values_to = "mse",
    names_pattern = "(.*)_"
  ) %>% 
  mutate(n_games = as.factor(n_games),
         n_comp = as.factor(n_comp),
         n_comp_per_game = as.factor(n_comp_per_game),
         sigma = as.factor(sigma),
         mse = as.numeric(mse))%>% 
  group_by(n_games, n_comp, n_comp_per_game, sigma, model) %>% 
  summarize(
    mean_mse = mean(mse, na.rm = TRUE),
    num_na = sum(is.na(mse))
  )
View(tidy_na_models)



#' Function that compute the expected amount of times two randomly picked up competitors  
#' will face each other in each scenario 
#' @param n_comp  total number of potential competitors
#' @param n_entrants number of entrants in each game
#' @param n_games number of games played
#' 
#' @returns the expected amount of times two randomly picked up competitors will face each other (density indicator)
p_pair_played <- function(n_comp, n_entrants, n_games) {
  
  total_connections <- n_comp * (n_comp + 1) / 2 - n_comp
  connections_per_game <- n_entrants * (n_entrants + 1) / 2 - n_entrants
  
  binom_prob <- connections_per_game / total_connections
  n_games * binom_prob
}

# creation of different grids that will be used to plot resutls

# grid where the density indicator is added to the results grid, and NA are removed
tidy_row <- grid_results_def_final %>% 
  filter(sim_data_error == FALSE) %>%
  drop_na() %>% 
  select(-sim_data_error) %>% 
  mutate( beta0_mse = as.numeric(beta0_mse),
         beta1_mse = as.numeric(beta1_mse),
         beta2_mse = as.numeric(beta2_mse),
         rol_mse = as.numeric (rol_mse), 
         sparsity = p_pair_played(n_comp, n_comp_per_game, n_games)) %>%
  mutate( n_games = as.factor(n_games),
         n_comp = as.factor(n_comp),
         n_comp_per_game = as.factor(n_comp_per_game),
         sigma = as.factor(sigma),
         sparisity_factor = as.factor(sparsity))

# pivot_longer is used to have one model per row
tidy <- tidy_row %>%
  pivot_longer(
    cols = ends_with("mse"),
    names_to = "model",
    values_to = "mse",
    names_pattern = "(.*)_"
  ) 




### grid where for each of the Beta regression models, the differences between 
# the MSE of the model and the MSE of the ROL model are computed
tidy_delta<- tidy_row %>%
  mutate(beta0_delta = beta0_mse - rol_mse,
         beta1_delta = beta1_mse - rol_mse,
         beta2_delta = beta2_mse - rol_mse) %>% 
  select(n_games, n_comp, n_comp_per_game, sigma, iter,beta0_delta, beta1_delta, beta2_delta,sparsity) %>% 
  pivot_longer(
    cols = ends_with("delta"),
    names_to = "model",
    values_to = "delta",
    names_pattern = "(.*)_"
  ) 

# assigning meaningful names to the levels of the factors
tidy$sigma <- fct_recode(tidy$sigma, "High-competitive" = "0.1", 
                         "Average-competitive" = "1", "Low-competitive" = "10")
tidy$model <- fct_recode(tidy$model, "ROL" = "rol", 
                         "Fixed-Beta" = "beta0", "Varying-Beta" = "beta1", "Dependent-Beta" = "beta2")

tidy_delta$sigma <- fct_recode(tidy_delta$sigma, "High-competitive" = "0.1", 
                         "Average-competitive" = "1", "Low-competitive" = "10")
tidy_delta$model <- fct_recode(tidy_delta$model,"Fixed-Beta" = "beta0", "Varying-Beta" = "beta1", "Dependent-Beta" = "beta2")

# saving the grids
saveRDS(tidy, "tidy.rds")
saveRDS(tidy_delta, "tidy_delta.rds")

## Table 2 ---------
# Reproduce Table 2 in the manuscript: Average MSE for all models across all conditions
tidy %>% 
  group_by(model) %>% 
  summarise(
    mean_mse = mean(mse, na.rm = TRUE),
  )
## FIGURES -------- 
### Figure 1----------------------
# reproduce Figure 1 in the manuscript: Boxplots of MSE achieved by ROL and Beta models, 
# by number of entrants and competitiveness levels, considering 50 total competitors and 
# 1250 played games
Figure1 <- tidy %>% 
  filter (n_games == 1250 & n_comp == 50) %>% 
  ggplot(aes(x=n_comp_per_game, y=mse, color = model)) + 
  geom_boxplot()+
  labs(title = "MSE by competitiveness levels and number of entrants, with 50 competitors and 1250 games",
       x = "Number of entrants",
       y = "MSE")+
  theme_minimal()+
  facet_wrap(.~sigma, nrow = 1, scales ="free") +
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 14 ),
        axis.title = element_text( size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 14))
Figure1

### Figure 2----------------------
# reproduce Figure 2 in the manuscript:   \caption{Boxplots of the the MSE achieved 
# by ROL and Beta models,  by number of games and competitiveness levels , considering 
# 50 potential competitors and 20 entrants in each game.
Figure2 <- tidy %>% 
  filter (n_comp == 50 & n_comp_per_game == 20) %>% 
  ggplot(aes(x=n_games, y=mse, color = model)) + 
  geom_boxplot()+
  labs(title = "MSE by competitiveness levels and number of games, with 50 competitors and 20 entrants",
       x = "Number of games",
       y = "MSE")+
  theme_minimal()+
  facet_wrap(.~sigma, nrow = 1, scales ="free") +
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 14 ),
        axis.title = element_text( size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 14))
Figure2

# looking at the differences in the MSE between the Beta models' MSE and the ROL's MSE for the same scenarios
tidy_delta %>% 
  filter (n_games == 1250 & n_comp == 50) %>% 
  ggplot(aes(x=n_comp_per_game, y=delta, color = model)) + 
  geom_boxplot()+
  labs(title = "Differences in the MSE between the Beta models' MSE and the ROL's MSE by competitiveness levels and number of entrants, with 50 competitors and 1250 games",
       x = "Number of entrants",
       y = "MSE")+
  theme_minimal()+
  facet_wrap(.~sigma, nrow = 1, scales ="free") +
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 14 ),
        axis.title = element_text( size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 14))


tidy_delta %>% 
  filter(n_comp == 50 & n_comp_per_game == 20) %>%
  ggplot(aes(x=n_games, y=delta, color = model)) + 
  geom_boxplot()+
  labs(title = "MSE differentiated by competitiveness levels and number of entrants, with 50 competitors and 1250 games",
       x = "Number of entrants",
       y = "MSE")+
  theme_minimal()+
  geom_hline(yintercept = 0, linetype = 2)+
  facet_wrap(.~sigma, nrow = 1, scales = "free") +
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 14 ),
        axis.title = element_text( size = 14),
        legend.position = "right",
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14))

### Figure 3 ----------------------
## reproduce Figure 3 in the manuscript: Average MSE achieved by all the models,
#for different amounts of density in the data, number of entrants and number of potential competitors
Figure3 <- tidy %>%
  group_by(n_games, n_comp, n_comp_per_game, sigma, model,sparsity) %>% 
  summarise(
    mean_mse = mean(mse)
  ) %>%
  rename(n_entrants = n_comp_per_game,
         tot_comp = n_comp) %>% 
  filter(sigma == "Low-competitive")%>% 
  ggplot(aes(x=as.factor(round(sparsity,2)), y=mean_mse, color = model)) + 
  geom_point(position = position_dodge(width = 0.5)) +
  labs(title = "Average MSE for different densities in the data, number of entrants and total number of competitors, \nconsidering average-competitive scenarios",
       x = "Density in the data",
       y = "MSE")+
  geom_line(aes(group = model),linetype="dashed" )+
  scale_y_log10()+
  facet_grid(vars(tot_comp),vars(n_entrants), scales = "free_x", labeller = "label_both")+
  theme_bw()+
  theme(axis.text.x = element_text(angle =70, hjust = 1))+
  theme(axis.text = element_text( size = 9 ),
        axis.text.x = element_text( size = 9),
        axis.title = element_text( size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 8),
        plot.title = element_text(size = 10))
Figure3

