# libraries needed
library(tidyverse)
library(cmdstanr)


## loading F2 data 
df <- readRDS("f2_tot_correct.rds")

## count how many times each driver compete throughout the seasons
df %>% group_by(Driver) %>% summarise(n = n())

# Names of drivers  who competed at least 30 times 
at_least_thirdy <- df %>% group_by(Driver) %>% summarise(n = n()) %>% filter(n > 30) %>% pull(Driver)

# histograms of finishing positions of the drivers who competed at least 30 times in 2020
df %>% filter(Driver %in% at_least_thirdy & season == 2020) %>% 
  ggplot(aes(x=Pos))+
  geom_histogram()+
  facet_wrap(.~Driver)+
  scale_fill_gradientn(colours = rainbow(7))+
  theme_minimal()+
  labs(
    x = "Finish position",
    y = "Count",
    title = "Different drivers' finish positions, considering just the drivers who joined at least 30 races")

## Running STAN models

## reading stan models 
rol_mod    <- cmdstan_model("stanfiles/rol_mod.stan")
beta0_mod  <- cmdstan_model("stanfiles/beta0_mod.stan")
beta1_mod  <- cmdstan_model("stanfiles/beta1_mod.stan")
beta2_mod  <- cmdstan_model("stanfiles/beta2_mod.stan")

## ROL model --------------------------------------------

# data specification for the ROL model
rol_data_f2<-list(
  num_obs   = nrow(df),
  num_comp  = length(unique(df$Driver)),
  num_games = length(unique(df$game_id)),
  ranked_comp_ids = as.factor(df %>%
  arrange(game_id, Pos) %>% 
  pull(Driver)),
  n_comp_per_game = df %>% 
    summarize(len = n(), .by = game_id) %>% 
    pull(len))

# sampling
fit_rol_f2 <- rol_mod$sample(
  data = rol_data_f2,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iterations
)
#All 4 chains finished successfully.
#Mean chain execution time: 17.5 seconds.
#Total execution time: 19.4 seconds.

fit_rol_f2$save_object("fit_rol_f2_correct.rds")


# retrieving the estimated abilities for the drivers who competed at least 30 times
rol_est <- tibble(Driver = levels(rol_data_f2$ranked_comp_ids),
                   theta = fit_rol_f2$summary(variables =c("theta"))$mean,
                  lower_ci = fit_rol_f2$summary(variables =c("theta"))$q5,
                  upper_ci = fit_rol_f2$summary(variables =c("theta"))$q95) %>%
  mutate(Driver = as.factor(Driver)) %>% 
  filter(Driver %in% at_least_thirdy) %>% 
                    arrange((theta)) 

# ordered names of the best drivers who compete at least 30 times, according to the ROL model
names_first_thirdy_ROL <- rol_est %>% pull(Driver)


# checking for convergence 
fit_rol_f2$diagnostic_summary(diagnostics=c("divergences","treedepth","ebfmi"))
print(fit_rol_f2$summary(variables = c("theta")), n=92)


## Fixed-Beta model --------------------------------------------

# data specification for the Fixed-Beta regression model
beta0_dat_f2 <- list(
  num_obs   = nrow(df),
  num_comp  = length(unique(df$Driver)),
  comp_id   = as.factor(df$Driver),
  y         = df$prop_beaten_transformed)

# sampling
fit_beta0_f2  <- beta0_mod$sample(
  data = beta0_dat_f2,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500,
  max_treedepth = 20# print update every 500 iters
)
#All 4 chains finished successfully.
#Mean chain execution time: 15.4 seconds.
#Total execution time: 16.0 seconds.
fit_beta0_f2$save_object("fit_beta0_f2_correct.rds")

# retrieving the estimated abilities for the drivers who competed at least 30 times
beta0_est <- tibble(Driver = levels(beta0_dat_f2$comp_id),
                  theta = fit_beta0_f2$summary(variables =c("theta_driver"))$mean,
                  lower_ci = fit_beta0_f2$summary(variables =c("theta_driver"))$q5,
                  upper_ci = fit_beta0_f2$summary(variables =c("theta_driver"))$q95) %>%
  mutate(Driver = as.factor(Driver)) %>% 
  filter(Driver %in% at_least_thirdy) %>% 
  arrange(desc(theta))

# checking for convergence 
fit_beta0_f2$diagnostic_summary(diagnostics=c("divergences","treedepth","ebfmi"))
print(fit_beta0_f2$summary(variables = c("theta_driver")), n=92)


## Varying-Beta model --------------------------------------------

# data specification for the Varying-Beta regression model
beta1_dat_f2 <- list(
  num_obs   = nrow(df),
  num_comp  = length(unique(df$Driver)),
  comp_id   = as.factor(df$Driver),
  y         = df$prop_beaten_transformed)

# sampling
fit_beta1_f2 <- beta1_mod$sample(
  data = beta1_dat_f2,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)
#All 4 chains finished successfully.
#Mean chain execution time: 31.9 seconds.
#Total execution time: 33.1 seconds.
fit_beta1_f2$save_object("fit_beta1_f2_correct.rds")

# retrieving the estimated abilities for the drivers who competed at least 30 times
beta1_est <- tibble(Driver = levels(beta1_dat_f2$comp_id),
                    theta = fit_beta1_f2$summary(variables =c("theta_driver"))$mean,
                    lower_ci = fit_beta1_f2$summary(variables =c("theta_driver"))$q5,
                    upper_ci = fit_beta1_f2$summary(variables =c("theta_driver"))$q95) %>%
  mutate(Driver = as.factor(Driver)) %>% 
  filter(Driver %in% at_least_thirdy) %>% 
  arrange(desc(theta)) 
beta1_est


# checking for convergence 
fit_beta1_f2$diagnostic_summary(diagnostics=c("divergences","treedepth","ebfmi"))
print(fit_beta1_f2$summary(variables = c("theta_driver")), n=92)


## Dependent-Beta regression model  --------------------------------------------

# data specification for the Dependent-Beta regression model
beta2_dat_f2 <- list(
  num_obs   = nrow(df),
  num_comp  = length(unique(df$Driver)),
  num_games = length(unique(df$game_id)),
  comp_id   = as.factor(df$Driver),
  game_id   = as.factor(df$game_id),
  y         = df$prop_beaten_transformed,
  n_comp_per_game = df %>% 
    summarize(len = n(), .by = game_id) %>% 
    pull(len))

#All 4 chains finished successfully.
#Mean chain execution time: 71.4 seconds.
#Total execution time: 73.9 seconds.

# sampling
fit_beta2_f2 <- beta2_mod$sample(
  data = beta2_dat_f2,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)

#All 4 chains finished successfully.
#Mean chain execution time: 71.4 seconds.
#Total execution time: 73.9 seconds.

fit_beta2_f2$save_object("fit_beta2_f2_correct.rds")



# retrieving the estimated abilities for the drivers who competed at least 30 times
beta2_est <- tibble(Driver = levels(beta2_dat_f2$comp_id),
                    theta = fit_beta2_f2$summary(variables =c("theta_driver"))$mean,
                    lower_ci = fit_beta2_f2$summary(variables =c("theta_driver"))$q5,
                    upper_ci = fit_beta2_f2$summary(variables =c("theta_driver"))$q95) %>%
               mutate(Driver = as.factor(Driver)) %>% 
              filter(Driver %in% at_least_thirdy) %>% 
               arrange(desc(theta)) 

# checking for convergence 
fit_beta2_f2$diagnostic_summary(diagnostics=c("divergences","treedepth","ebfmi"))
print(fit_beta2_f2$summary(variables = c("theta_driver")), n=92)





