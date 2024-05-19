# libraries needed 
library(tidyverse)
library(cmdstanr)
library(pbapply)
library(parallel)



# seed for reproducibility purposes
set.seed(123)

#' Function that sample values from a Gumbel distribution 
#' 
#' @param theta A vector of n ability parameters where n is the number of competitors
#' @returns A vector of n values sampled from a Gumbel distribution, corresponding competitors' performances
rgumbel <- function(theta) {
  y <- runif(length(theta))
  theta - (log(-log(y)))
}

#' Function that checks if the combination of factors in the simulation study is feasible 
#' 
#' @param n_games number of simulated games
#' @param n_comp number of total competitors
#' @param n_comp_per_game number of entrants per game
#' 
#' @returns an error message if the combination of factors in the simulation study is not feasible
check_arg <-  function(n_games = 50, n_comp = 15, n_comp_per_game = 2){
  if (n_games * n_comp_per_game < n_comp) {
    stop("not all competitors compete at least once")
  }
  if (n_comp_per_game > n_comp) {
    stop("more entrants per game than total number of competitors")
  }
}
# check_arg(n_games = 50, n_comp = 8, n_comp_per_game = 15) # ok 
# check_arg(n_games = 2, n_comp = 5, n_comp_per_game = 2) # error


#' Function that create a matrix of competitors' indices, that will be used later to sample competitors' indices in each game 
#' 
#' @param n_games number of simulated games
#' @param n_comp number of total competitors
#' @param n_comp_per_game number of entrants per game
#' 
#' @returns a competitor indices matrix if the sampling algortihm was able to meet with the condition
create_compids_mat <- function (n_games = 50, n_comp = 15, n_comp_per_game = 2){
  compids_mat <- t(sapply(1:(n_games), \(x) sample(1:n_comp, n_comp_per_game, replace = FALSE)))
  if (length(unique(c(compids_mat))) < n_comp) {
    return(create_compids_mat(n_games, n_comp, n_comp_per_game))
  }
  return(compids_mat)
}
# compids_mat_example <- create_compids_mat(n_games = 50, n_comp = 15, n_comp_per_game = 2) 
# !length(unique(compids_mat_example)) == 15 # ok


#' Function that simulate finishing positions of competitors according to a Rank-Ordered Logit model.
#' 
#' @param n_games number of simulated games
#' @param n_comp number of total competitors
#' @param n_comp_per_game number of entrants per game
#' @param sigma standard deviation of abilities, representing competitiveness level in the games
#' 
#' @returns A list containing: 1. A vector "theta" of competitors' abilities,
#'                             2. A data frame "df" of simulated finishing positions of competitors in n_games games
simulate_data <- function(n_games = 50, n_comp = 15, n_comp_per_game = 2, sigma = 0.1) {
  check_arg(n_games, n_comp, n_comp_per_game)
  thetas  <- rnorm(n_comp, 0, sigma)
  data    <- matrix(integer(), n_games, n_comp_per_game)
  game_id <- 1:n_games
  compids_mat <- create_compids_mat(n_games, n_comp, n_comp_per_game)
  for (i in game_id) {
    comp_ids <- compids_mat[i,]
    performances <- rgumbel(thetas[comp_ids])
    results  <- comp_ids[order(performances, decreasing = TRUE)]
    data[i,] <- results
  }
  colnames(data) <- paste0("rank", 1:n_comp_per_game)
  data_final     <- cbind(game_id,data)
  return(list(
    theta = thetas,
    df = as.data.frame(data_final)
  ))
}
# simulate_data(n_games = 2, n_comp = 5, n_comp_per_game = 2, sigma = 1) # error, as expected
# simulate_data(n_games = 50, n_comp = 15, n_comp_per_game = 2, sigma = 0.1)$df # ok


#' Function that transform the data simulated in simulate_data into a format that can be used as input in the Stan models
#' 
#' @param data_sim output of the function simulate_data
#' 
#' @returns a tidy data-frame where each row corresponds to a competitor in a game and with the following columns:
#'          1. game_id : game identifier
#'          2. comp_id : competitor identifier
#'          3. position : finishing position of the competitor in the game
#'          4. prop_beaten : proportion of outperformed competitors
#'          5. prop_beaten_transformed : transformed proportion of outperformed competitors
prep_data <- function(data_sim) {
  n_comp  <- length(data_sim$theta) 
  df_sim  <- data_sim$df
  n_comp_per_game <- (ncol(df_sim)-1)
  df_tidy <- pivot_longer(
    data      = df_sim,
    cols      = paste0("rank", 1:n_comp_per_game),
    names_to  = "position",
    values_to = "comp_id"
  )
  df_tidy$position    <- as.numeric(gsub("rank", "", df_tidy$position))
  df_tidy$prop_beaten <- (n_comp_per_game - df_tidy$position) / (n_comp_per_game - 1)
  df_tidy$prop_beaten_transformed <- (df_tidy$prop_beaten * (n_comp_per_game - 1) + 0.5) / (n_comp_per_game)
  df_tidy$comp_id <- as.factor(df_tidy$comp_id)
  df_tidy$game_id <- as.factor(df_tidy$game_id)
  return(df_tidy)
}


## reading stan models 
rol_mod    <- cmdstan_model("stanfiles/rol_mod.stan")
beta0_mod  <- cmdstan_model("stanfiles/beta0_mod.stan")
beta1_mod  <- cmdstan_model("stanfiles/beta1_mod.stan")
beta2_mod  <- cmdstan_model("stanfiles/beta2_mod.stan")

#' Function that applies the Fixed-Beta Stan model to the dataset generated from prep_data() 
#' @param df_tidy output of the function prep_data()
#' 
#' @returns an object called fit_beta0 that contains the estimated Fixed-Beta Stan model
run_beta0_mod <- function(df_tidy) {
  beta0_dat <- list(
    num_obs   = nrow(df_tidy),
    num_comp  = nlevels(df_tidy$comp_id),
    comp_id   = df_tidy$comp_id,
    y         = df_tidy$prop_beaten_transformed
  )
  fit_beta0 <- beta0_mod$optimize(data = beta0_dat, jacobian = TRUE)
  fit_beta0
}

#' Function that applies the Varying-Beta Stan model to the dataset generated from prep_data() 
#' @param df_tidy output of the function prep_data()
#' 
#' @returns an object called fit_beta1 that contains the estimated Varying-Beta Stan model
run_beta1_mod <- function(df_tidy) {
  beta1_dat <- list(
    num_obs   = nrow(df_tidy),
    num_comp  = nlevels(df_tidy$comp_id),
    comp_id   = df_tidy$comp_id,
    y         = df_tidy$prop_beaten_transformed
  )
  fit_beta1 <- beta1_mod$optimize(data = beta1_dat, jacobian = TRUE)
  fit_beta1
}


#' Function that applies the Dependent-Beta Stan model to the dataset generated from prep_data() 
#' @param df_tidy output of the function prep_data()
#' 
#' @returns an object called fit_beta2 that contains the estimated Dependent-Beta Stan model
run_beta2_mod <- function(df_tidy) {
  beta2_dat<- list(
    num_obs   = nrow(df_tidy),
    num_comp  = nlevels(df_tidy$comp_id),
    num_games = length(unique(df_tidy$game_id)),
    comp_id   = df_tidy$comp_id,
    game_id   = df_tidy$game_id,
    y         = df_tidy$prop_beaten_transformed,
    n_comp_per_game = df_tidy %>% 
      summarize(len = n(), .by = game_id) %>% 
      pull(len)
  )
  fit_beta2 <- beta2_mod$optimize(data = beta2_dat, jacobian = TRUE)
  fit_beta2
}


#' Function that applies the ROL Stan model to the dataset generated from prep_data() 
#' @param df_tidy output of the function prep_data()
#' 
#' @returns an object called fit_rol that contains the estimated ROL Stan model
run_rol_mod <- function(df_tidy) {
  rol_dat <- list(
    num_obs   = nrow(df_tidy),
    num_comp  = nlevels(df_tidy$comp_id),
    num_games = length(unique(df_tidy$game_id)),
    ranked_comp_ids = df_tidy %>%
      arrange(game_id, position) %>% 
      pull(comp_id),
    n_comp_per_game = df_tidy %>% 
      summarize(len = n(), .by = game_id) %>% 
      pull(len))
  fit_rol <- rol_mod$optimize(data = rol_dat, jacobian = TRUE)
  fit_rol
}

# creating the grid where each row corresponds to a different iteration in the simulation study
# 1000 iterations for each condition
grid_thousand_it <- expand.grid(
  n_games         = c(50, 250, 500, 1250),
  n_comp          = c(15, 50, 250, 500),
  n_comp_per_game = c(2, 8, 20, 40),
  sigma           = c(0.1, 1, 10),
  iter            = 1:1000
)

#' Function that checks if the combination of factors in the grid is doable on the base of the previously created "check_grid"
#' @param row_id  each row in the grid
#' 
#' @returns TRUE if the combination is doable, FALSE if it's not doable 
check_arg_grid <- function(row_id){
  par <- grid_thousand_it[row_id,]
  check <- try(check_arg(par$n_games, par$n_comp, par$n_comp_per_game), silent = TRUE)
  if (inherits(check, "try-error")){
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# updating of the grid, excluding the unfeasible rows
grid_thousand_it <- grid_thousand_it %>% 
  mutate(par_check = pbsapply(1:nrow(grid_thousand_it), check_arg_grid))%>% 
  filter(par_check == TRUE) %>% 
  select(-par_check)

# save the grid
saveRDS(grid_thousand_it,"grid_thousand_it.rds")


#' Function that compute the mean squared error between the true abilities and the estimated ones 
#' @param est_sum  summary of estimated parameters from a stan model
#' @param true_abilities  true abilities of the competitors
#' 
#' @returns the mean squared error between the true abilities and the estimated ones
compute_mse <- function(est_sum, true_abilities){
  est <- collect(select(est_sum, estimate))[[1]]
  mse <- mean((est-true_abilities)^2)
  mse
}

#' Function that generate the dataset according to the simulation parameters and directly
#' computes the mean squared error between the true abilities and the estimated ones
#' @param row_id  row in the grid of reference
#' 
#' @returns a vector with the mean squared error for the models if the simulation algorithm 
#' works, or a vector of NA if the simulation algorithm fails 
run_sim <- function(row_id) {
  par       <- grid_thousand_it[row_id,]
  data_sim  <- try(simulate_data(par$n_games, par$n_comp, par$n_comp_per_game, par$sigma), silent = TRUE)
  if (inherits(data_sim, "try-error")) {return(c(NA, NA, NA, NA, "TRUE"))} else df_tidy <- prep_data(data_sim)
  true_ab    <- data_sim$theta
  beta0      <- run_beta0_mod(df_tidy)
  if (beta0$return_codes() == 0) beta0_est_sum <- beta0$summary(variables=c("theta_driver"))
  rol        <- run_rol_mod(df_tidy)
  if (rol$return_codes() == 0) rol_est_sum <- rol$summary(variables=c("theta"))
  beta1      <- run_beta1_mod(df_tidy)
  if (beta1$return_codes() == 0) beta1_est_sum <- beta1$summary(variables=c("theta_driver"))
  beta2      <- run_beta2_mod(df_tidy)
  if (beta2$return_codes() == 0) beta2_est_sum <- beta2$summary(variables=c("theta_driver"))
  mse_beta0  <- if (exists("beta0_est_sum")) compute_mse(beta0_est_sum, true_ab) else NA
  mse_beta1  <- if (exists("beta1_est_sum")) compute_mse(beta1_est_sum, true_ab) else NA
  mse_beta2  <- if (exists("beta2_est_sum")) compute_mse(beta2_est_sum, true_ab) else NA
  mse_rol    <- if (exists("rol_est_sum")) compute_mse(rol_est_sum, true_ab) else NA
  return(c(mse_beta0, mse_beta1, mse_beta2, mse_rol, "FALSE"))
}


## Running parallel simulations
clus <- makeForkCluster(44)
sim_results <- pbsapply(1:159000,run_sim, cl = clus)
stopCluster(clus)
## 

# creating the grid results, adding one column for each MSE and one for the simulation error indicator 
# (TRUE if the simulation algorithm fails, FALSE otherwise)
grid_results_thousand_it <- grid_thousand_it%>% 
  mutate(beta0_mse = vector(length = nrow(grid_thousand_it)),
         beta1_mse = vector(length = nrow(grid_thousand_it)),
         beta2_mse = vector(length = nrow(grid_thousand_it)),
         rol_mse = vector(length = nrow(grid_thousand_it)),
         sim_data_error = vector(mode = "logical", length = nrow(grid_thousand_it)))

# adding the MSE returned by run_sim() function to the grid results
grid_results_thousand_it[1:159000, c("beta0_mse","beta1_mse","beta2_mse","rol_mse","sim_data_error")] <- t(sim_results)

# saving the grid
saveRDS(grid_results_thousand_it,"grid_results_def_final.rds")



