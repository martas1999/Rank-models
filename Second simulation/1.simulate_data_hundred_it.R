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
#' @param n_entrants number of entrants per game
#' 
#' @returns an error message if the combination of factors in the simulation study is not feasible
check_arg <-  function(n_games = 50, n_comp = 15, n_entrants = 2){
  if (n_games * n_entrants < n_comp) {
    stop("not all competitors compete at least once")
  }
  if (n_entrants > n_comp) {
    stop("more entrants per game than total number of competitors")
  }
}
# check_arg(n_games = 50, n_comp = 8, n_entrants = 15) # ok 
# check_arg(n_games = 2, n_comp = 5, n_entrants = 2) # error


#' Function that create a matrix of competitors' indices, that will be used later to sample competitors' indices in each game 
#' 
#' @param n_games number of simulated games
#' @param n_comp number of total competitors
#' @param n_entrants number of entrants per game
#' 
#' @returns a competitor indices matrix if the sampling algortihm was able to meet with the condition
create_compids_mat <- function (n_games = 50, n_comp = 15, n_entrants = 2){
  compids_mat <- t(sapply(1:(n_games), \(x) sample(1:n_comp, n_entrants, replace = FALSE)))
  if (length(unique(c(compids_mat))) < n_comp) {
    return(create_compids_mat(n_games, n_comp, n_entrants))
  }
  return(compids_mat)
}
# compids_mat_example <- create_compids_mat(n_games = 50, n_comp = 15, n_entrants = 2) 
# !length(unique(compids_mat_example)) == 15 # ok

#' Function that simulate finishing positions of competitors according to a Rank-Ordered Logit model.
#' 
#' @param n_games number of simulated games
#' @param n_comp number of total competitors
#' @param n_entrants number of entrants per game
#' @param sigma standard deviation of abilities, representing competitiveness level in the games
#' 
#' @returns A list containing: 1. A vector "theta" of competitors' abilities,
#'                             2. A data frame "df" of simulated finishing positions of competitors in n_games games
simulate_data <- function(n_games = 50, n_comp = 15, n_entrants = 2, sigma = 0.1) {
  check_arg(n_games, n_comp, n_entrants)
  thetas  <- rnorm(n_comp, 0, sigma)
  data    <- matrix(integer(), n_games, n_entrants)
  game_id <- 1:n_games
  compids_mat <- create_compids_mat(n_games, n_comp, n_entrants)
  for (i in game_id) {
    comp_ids <- compids_mat[i,]
    performances <- rgumbel(thetas[comp_ids])
    results  <- comp_ids[order(performances, decreasing = TRUE)]
    data[i,] <- results
  }
  colnames(data) <- paste0("rank", 1:n_entrants)
  data_final     <- cbind(game_id,data)
  return(list(
    theta = thetas,
    df = as.data.frame(data_final)
  ))
}
# simulate_data(n_games = 2, n_comp = 5, n_entrants = 2, sigma = 1) # error, as expected
# simulate_data(n_games = 50, n_comp = 15, n_entrants = 2, sigma = 0.1)$df # ok


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
  n_entrants <- (ncol(df_sim)-1)
  df_tidy <- pivot_longer(
    data      = df_sim,
    cols      = paste0("rank", 1:n_entrants),
    names_to  = "position",
    values_to = "comp_id"
  )
  df_tidy$position    <- as.numeric(gsub("rank", "", df_tidy$position))
  df_tidy$prop_beaten <- (n_entrants - df_tidy$position) / (n_entrants - 1)
  df_tidy$prop_beaten_transformed <- (df_tidy$prop_beaten * (n_entrants - 1) + 0.5) / (n_entrants)
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
    n_entrants = df_tidy %>% 
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
    n_entrants = df_tidy %>% 
      summarize(len = n(), .by = game_id) %>% 
      pull(len))
  fit_rol <- rol_mod$optimize(data = rol_dat, jacobian = TRUE)
  fit_rol
}

# creating the grid where each row corresponds to a different iteration in the simulation study
# 100 iterations for each condition
grid_hundred_it <- expand.grid(
  n_games         = c(50, 250, 500, 1250),
  n_comp          = c(15, 50, 250, 500),
  n_entrants = c(2, 8, 20, 40),
  sigma           = c(0.1, 1, 10),
  iter            = 1:100
)


#' Function that checks if the combination of factors in the grid is doable on the base of the previously created "check_grid"
#' @param row_id  each row in the grid
#' 
#' @returns TRUE if the combination is doable, FALSE if it's not doable 
check_arg_grid <- function(row_id){
  par <- grid_hundred_it[row_id,]
  check <- try(check_arg(par$n_games, par$n_comp, par$n_entrants), silent = TRUE)
  if (inherits(check, "try-error")){
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# updating of the grid, excluding the unfeasible rows
grid_hundred_it <- grid_hundred_it %>% 
  mutate(par_check = pbsapply(1:nrow(grid_hundred_it), check_arg_grid))%>% 
  filter(par_check == TRUE) %>% 
  select(-par_check)

# save the grid
saveRDS(grid_hundred_it,"grid_hundred_it.rds")


#' Function that creates the name of the file where the output of the estimated Fixed-Beta model will be stored
#' @param par  the combination of simulation parameters, corresponding to each row in the grid
#' 
#' @returns the name of the file 
create_file_name_beta0 <- function(par){
  filename_prefix <- paste0("sim_output/", paste(par, collapse = "_"))
  beta0_file <- paste0(filename_prefix, "_beta0.rds")
  beta0_file
}


#' Function that creates the name of the file where the output of the estimated Varying-Beta model will be stored
#' @param par  the combination of simulation parameters, corresponding to each row in the grid
#' 
#' @returns the name of the file 
create_file_name_beta1 <- function(par){
  filename_prefix <- paste0("sim_output/", paste(par, collapse = "_"))
  beta1_file <- paste0(filename_prefix, "_beta1.rds")
  beta1_file
}

#' Function that creates the name of the file where the output of the estimated Dependent-Beta model will be stored
#' @param par  the combination of simulation parameters, corresponding to each row in the grid
#' 
#' @returns the name of the file 
create_file_name_beta2 <- function(par){
  filename_prefix <- paste0("sim_output/", paste(par, collapse = "_"))
  beta2_file <- paste0(filename_prefix, "_beta2.rds")
  beta2_file
}


#' Function that creates the name of the file where the output of the estimated ROL model will be stored
#' @param par  the combination of simulation parameters, corresponding to each row in the grid
#' 
#' @returns the name of the file 
create_file_name_rol <- function(par){
  filename_prefix <- paste0("sim_output/", paste(par, collapse = "_"))
  rol_file  <- paste0(filename_prefix, "_rol.rds")
  rol_file
}

#' Function that creates the name of the file where the vector of real abilities is stored
#' @param par  the combination of simulation parameters, corresponding to each row in the grid
#' 
#' @returns the name of the file 
create_file_name_abilities <- function(par){
  ab_name_prefix <- paste0("sim_abilities/", paste(par, collapse = "_"))
  ab_file <- paste0(ab_name_prefix, "_ab.rds")
  ab_file
}

#' Function that creates the name of the file where the data-frame is stored
#' @param par  the combination of simulation parameters, corresponding to each row in the grid
#' 
#' @returns the name of the file 
create_file_name_df <- function(par){
  dfname_prefix <- paste0("sim_df/", paste(par, collapse = "_"))
  df_file   <- paste0(dfname_prefix, "_df.rds")
  df_file
}


#' Function that save the estimated models, the vector of true abilities and the generated data-frame for each
#' geenrated dataset in each row in the grid. Only those datasets for which simulate_data() works will be used 
#' to run the STAN models and only estimated models without divergences are saved. 
#' @param row_id  one row in the grid
#' 
#' @returns the name of the file 
run_sim <- function(row_id) {
  par       <- grid_hundred_it[row_id,]
  ab_file    <- create_file_name_abilities(par)
  df_file    <- create_file_name_df(par)
  beta0_file <- create_file_name_beta0(par)
  beta1_file <- create_file_name_beta1(par)
  beta2_file <- create_file_name_beta2(par)
  rol_file   <- create_file_name_rol(par)
  data_sim  <- try(simulate_data(par$n_games, par$n_comp, par$n_entrants, par$sigma), silent = TRUE)
  if (inherits(data_sim, "try-error")) { return() }
  df_tidy    <- prep_data(data_sim)
  true_ab    <- data_sim$theta
  saveRDS(df_tidy, file = df_file)
  saveRDS(true_ab, file = ab_file)
  beta0      <- run_beta0_mod(df_tidy)
  if (beta0$return_codes() == 0) {saveRDS(beta0$summary(variables=c("theta_driver")), file =beta0_file)}
  rol        <- run_rol_mod(df_tidy)
  if (rol$return_codes() == 0) {saveRDS(rol$summary(variables=c("theta")), file =rol_file)}
  beta1      <- run_beta1_mod(df_tidy)
  if (beta1$return_codes() == 0) {saveRDS(beta1$summary(variables=c("theta_driver")), file =beta1_file)}
  beta2      <- run_beta2_mod(df_tidy)
  if (beta2$return_codes() == 0) {saveRDS(beta2$summary(variables=c("theta_driver")), file =beta2_file)}
}
# checking again the number of rows in the grid 
nrow(grid_hundred_it)

## Running parallel simulations
clus <- makeForkCluster(42)
pbsapply(1:15900,run_sim, cl = clus)
stopCluster(clus)

