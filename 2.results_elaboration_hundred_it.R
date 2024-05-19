# libraries needed
library(parallel)
library(pbapply)
library(tidyverse)
library(Rankcluster)


# read the grid
grid_hundred_it <- readRDS("grid_hundred_it.rds")


# extending the grid to incorporate columns for results 
grid_results_hundred_it_intermediate <- grid_hundred_it %>% 
  mutate(error_rol = vector(length = nrow(grid_hundred_it)),
         error_beta0 = vector(length = nrow(grid_hundred_it)),
         error_beta1 = vector(length = nrow(grid_hundred_it)),
         error_beta2 = vector(length = nrow(grid_hundred_it)),
         delta_error_beta0 = vector(length = nrow(grid_hundred_it)),
         delta_error_beta1 = vector(length = nrow(grid_hundred_it)),
         delta_error_beta2 = vector(length = nrow(grid_hundred_it)),
         identity_beta12 = vector(length = nrow(grid_hundred_it)),
         sim_data_error = vector(length = nrow(grid_hundred_it)))


#' Function that creates the name of the file where the output of the estimated Dependent-Beta model will be stored
#' @param model_file  object where the estimated model is stored
#' @param true_rank  vector of competitor indices as ranked according to their true abilities 
#' 
#' @returns the number of times the rank for a specific competitors is equal between the real rank and the estimated one
rank_check_true <- function(model_file,true_rank){
  model_ab <-model_file$estimate
  names(model_ab)<-c(1:length(model_ab))
  model_rank <- names(sort(model_ab, decreasing = TRUE))
  error_model <- sum(true_rank == model_rank)
  error_model
}

#' Function that creates the name of the file where the output of the estimated Dependent-Beta model will be stored
#' @param model_one  object where first estimated model is stored
#' @param model_two object where second estimated model is stored
#' 
#' @returns the number of times the rank for a specific competitors is equal between estimated rankd by the two models
rank_check_models <- function(model_one,model_two){
  model_one_ab <- model_one$estimate
  model_two_ab <- model_two$estimate
  names(model_one_ab)<-c(1:length(model_one_ab))
  names(model_two_ab)<-c(1:length(model_two_ab))
  model_one_rank <- names(sort(model_one_ab, decreasing = TRUE))
  model_two_rank <- names(sort(model_two_ab, decreasing = TRUE))
  error_rank <- sum(model_one_rank == model_two_rank)
}


#' Function that evaluates the number of correctly ranked competitors between the true ranking and the estimated ones for each model
#' and the number of correctly ranked competitors between the ROL rankink and the Beta Regression models.
#' @param row_id  the row if of the gird
#' @param grid  the grid of simulation parameters
#' 
#' @returns a vector of NA if the algorithm was not able to produce the dataset associated to the simulation parameters in the row;
#' otherwise, it returns the number of correctly ranked competitors between the true ranking and the estimated ones for each model.
check_error_rank <- function(row_id, grid){
  par <- grid[row_id,]
  beta0_file <- create_file_name_beta0(par)
  beta1_file <- create_file_name_beta1(par)
  beta2_file <- create_file_name_beta2(par)
  rol_file   <- create_file_name_rol(par)
  ab_file    <- create_file_name_abilities(par)
  if (file.exists(ab_file)) {ab <- readRDS(ab_file)} else {return(c(NA, NA, NA, NA, NA, NA, NA, NA, TRUE))}
  names(ab)<-c(1:length(ab))
  true_rank <- names(sort(ab, decreasing = TRUE))
  error_rol <- if (file.exists(rol_file)) rank_check_true(readRDS(rol_file),true_rank) else NA
  error_beta0 <- if (file.exists(beta0_file)) rank_check_true(readRDS(beta0_file),true_rank) else NA
  error_beta1 <-  if (file.exists(beta1_file)) rank_check_true(readRDS(beta1_file),true_rank) else NA
  error_beta2 <- if (file.exists(beta2_file)) rank_check_true(readRDS(beta2_file),true_rank) else NA
  delta_error_beta0 <- if (file.exists(beta0_file)) rank_check_models(readRDS(beta0_file),readRDS(rol_file)) else NA
  delta_error_beta1 <- if (file.exists(beta1_file)) rank_check_models(readRDS(beta1_file),readRDS(rol_file)) else NA
  delta_error_beta2 <- if (file.exists(beta2_file)) rank_check_models(readRDS(beta2_file),readRDS(rol_file)) else NA
  identity_beta12 <- if (file.exists(beta1_file) & file.exists(beta2_file)) rank_check_models(readRDS(beta2_file),readRDS(beta1_file)) else NA
  return(c(error_rol, error_beta0, error_beta1, error_beta2, delta_error_beta0, delta_error_beta1, delta_error_beta2,identity_beta12, FALSE ))
}

## running the function check_error_rank in parallel 
clus <- makeForkCluster(40)
results <- pbsapply(1:15900, check_error_rank, grid = grid_hundred_it,cl =clus)
stopCluster(clus)

## store the results from the rank_check_tot() function in the corresponding columns in the previously created 
grid_results_hundred_it_intermediate[1:15900, c("error_rol", "error_beta0", "error_beta1", "error_beta2", "delta_error_beta0", "delta_error_beta1", "delta_error_beta2","identity_beta12","sim_data_error")] <- t(results)


#' Function that extracts the estimated abilities from the estimated Stan model
#' @param model  object where the estimated models is stored
#' 
#' @returns a vector of estimated abilities
extract_estimates <-function(model){
  abilities <- model$estimate
  abilities
}


#' Function that extracts the estimated ranking of competitors from the estimated Stan model
#' @param model  object where the estimated models is stored
#' 
#' @returns a vector of competitor indices indicating the rankings of competitors according to the estimated abilities.
extract_ranks <- function(model){
  abilities <- model$estimate
  names(abilities)<-c(1:length(abilities))
  rank <- names(sort(abilities, decreasing = TRUE))
  rank 
}

#' Function that extracts the estimated ranking of competitors from the estimated Stan model
#' @param model  object where the estimated models is stored
#' 
#' @returns if the simulation algortihm worked for the combination of facotrs, 
#' function returns vectors of true abiltieis and estiamted abilities (if model converged)
#'  and vectors of competitor indices indicating the rankings of competitors 
#'  according to the estimated abilities and true abiltiies
retrieve_rank <- function(row_id, grid){
  par <- grid[row_id,]
  beta0_file <- create_file_name_beta0(par)
  beta1_file <- create_file_name_beta1(par)
  beta2_file <- create_file_name_beta2(par)
  rol_file   <- create_file_name_rol(par)
  ab_file    <- create_file_name_abilities(par)
  if (file.exists(ab_file)) {ab <- readRDS(ab_file)} else {return(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))}
  names(ab)<-c(1:length(ab))
  true_rank <- names(sort(ab, decreasing = TRUE))
  rol_ab <-  if (file.exists(rol_file)) extract_estimates(readRDS(rol_file)) else NA
  beta0_ab <- if (file.exists(beta0_file)) extract_estimates(readRDS(beta0_file)) else NA
  beta1_ab <-  if (file.exists(beta1_file)) extract_estimates(readRDS(beta1_file))else NA
  beta2_ab <- if (file.exists(beta2_file)) extract_estimates(readRDS(beta2_file)) else NA
  rol_rank <- if (file.exists(rol_file)) extract_ranks(readRDS(rol_file)) else NA
  beta0_rank <- if (file.exists(beta0_file)) extract_ranks(readRDS(beta0_file)) else NA
  beta1_rank <- if (file.exists(beta1_file)) extract_ranks(readRDS(beta1_file)) else NA
  beta2_rank <- if (file.exists(beta2_file)) extract_ranks(readRDS(beta2_file)) else NA
  return(list(abilities = ab,true_ranking =true_rank, rol_estimates = rol_ab, beta0_estimates = beta0_ab, beta1_estimates = beta1_ab, beta2_estimates = beta2_ab,rol_rank = rol_rank, beta0_rank =beta0_rank, beta1_rank = beta1_rank, beta2_rank = beta2_rank))
}

# running the function retrieve_rank in parallel
results <- pbsapply(1:15900,retrieve_rank, grid = grid_hundred_it)

# transposing the results and converting it into a dataframe
results_ranking <- t(results)
results_ranking_tibble <- as.data.frame(results_ranking)

# combining together the results from the rank_check_tot() function and the retrieve_rank() function
grid_results_hundred_it <- cbind(grid_results_hundred_it_intermediate,results_ranking_tibble)

# save the results grid
saveRDS(grid_results_hundred_it,"grid_results_hundred_it.rds")



# analysis of the simulation algorithm failures 
sim_failures_hundred <-grid_results_hundred_it %>% 
  filter(sim_data_error == 1) %>%
  group_by(n_games, n_comp, n_comp_per_game, sigma) %>% 
  summarise(
    count = n())
View(sim_failures_hundred)

# analysis of non converged estimated beta models
betas_failures_hundred <-grid_results_hundred_it %>% 
  filter(sim_data_error == 0) %>% 
  mutate(n_games = as.factor(n_games),
         n_comp = as.factor(n_comp),
         n_comp_per_game = as.factor(n_comp_per_game),
         sigma = as.factor(sigma),
         beta0_error = as.numeric(error_beta0),
         beta1_error = as.numeric(error_beta1),
         beta2_error = as.numeric(error_beta2),
         rol_error= as.numeric (error_rol)) %>%
  select((n_games:sigma), (beta0_error:rol_error)) %>% 
  pivot_longer(
    cols = ends_with("error"),
    names_to = "model",
    values_to = "guessed",
    names_pattern = "(.*)_") %>% 
  filter(is.na(guessed)) %>% 
  group_by(n_games, n_comp, n_comp_per_game, sigma, model) %>% 
  summarise(
    count = n())

View(betas_failures_hundred)



# grid to be used later on where only the simulation factors and the true and estimated rankings are stores
rankings_grid <- grid_results_hundred_it %>% 
  filter(sim_data_error == 0) %>% 
  drop_na() %>% 
  select(c("n_games","n_comp","n_comp_per_game","sigma","iter","true_ranking", "beta0_rank", "rol_rank","beta1_rank","beta2_rank"))




#' Function that computes the normalised Kendall distances between the estimated ROL ranking of competitors and the true ranking
#' @param rankings_grid grid as defined above
#' 
#' @returns the normalised Kendall distance between the estimated ranking of competitors and the true ranking
kd_rol<- function(rankings_grid){
  distances <- vector(length = nrow(rankings_grid))
  for (i in 1:nrow(grid)) {
    model_rank <- as.numeric(rankings_grid$rol_rank[[i]])
    true_rank <- as.numeric(rankings_grid$true_ranking[[i]])
    dist_not_norm <- distKendall(model_rank, true_rank)
    distances[i]<-(dist_not_norm)/(0.5*length(model_rank)*(length(model_rank)-1))
  }
  distances
}

#' Function that computes the normalised Kendall distances between the estimated Fixed-Beta ranking of competitors and the true ranking
#' @param rankings_grid grid as defined above
#' 
#' @returns the normalised Kendall distance between the estimated ranking of competitors and the true ranking
kd_beta0<- function(rankings_grid){
  distances <- vector(length = nrow(rankings_grid))
  for (i in 1:nrow(rankings_grid)) {
    model_rank <- as.numeric(rankings_grid$beta0_rank[[i]])
    true_rank <- as.numeric(rankings_grid$true_ranking[[i]])
    dist_not_norm <- distKendall(model_rank, true_rank)
    distances[i]<-(dist_not_norm)/(0.5*length(model_rank)*(length(model_rank)-1))
  }
  distances
}

#' Function that computes the normalised Kendall distances between the estimated Varying-Beta ranking of competitors and the true ranking
#' @param rankings_grid grid as defined above
#' 
#' @returns the normalised Kendall distance between the estimated ranking of competitors and the true ranking
kd_beta1<- function(rankings_grid){
  distances <- vector(length = nrow(rankings_grid))
  for (i in 1:nrow(rankings_grid)) {
    model_rank <- as.numeric(rankings_grid$beta1_rank[[i]])
    true_rank <- as.numeric(rankings_grid$true_ranking[[i]])
    dist_not_norm <- distKendall(model_rank, true_rank)
    distances[i]<-(dist_not_norm)/(0.5*length(model_rank)*(length(model_rank)-1))
  }
  distances
}


#' Function that computes the normalised Kendall distances between the estimated Dependent-Beta ranking of competitors and the true ranking
#' @param rankings_grid grid as defined above
#' 
#' @returns the normalised Kendall distance between the estimated ranking of competitors and the true ranking
kd_beta2<- function(rankings_grid){
  distances <- vector(length = nrow(rankings_grid))
  for (i in 1:nrow(rankings_grid)) {
    model_rank <- as.numeric(rankings_grid$beta2_rank[[i]])
    true_rank <- as.numeric(rankings_grid$true_ranking[[i]])
    dist_not_norm <- distKendall(model_rank, true_rank)
    distances[i]<-(dist_not_norm)/(0.5*length(model_rank)*(length(model_rank)-1))
  }
  distances
}

# adding to the grid four columns, each of them containing the normalised Kendall distance 
# for the different models
rankings_grid_kendall <- rankings_grid %>% 
  mutate(rol_kendall = kd_rol(rankings_grid),
         beta0_kendall = kd_beta0(rankings_grid),
         beta1_kendall = kd_beta1(rankings_grid),
         beta2_kendall = kd_beta2(rankings_grid))

# save it 
saveRDS(rankings_grid_kendall, "rankings_grid_kendall.rds")

# transform the grid in such a way for each row there is one model
grid_kendall_def <- rankings_grid_kendall %>% 
  mutate(n_games = as.factor(n_games),
         n_comp = as.factor(n_comp),
         n_comp_per_game = as.factor(n_comp_per_game),
         sigma = as.factor(sigma)) %>%
  select((n_games:sigma), (rol_kendall:beta2_kendall)) %>% 
  pivot_longer(
    cols = ends_with("kendall"),
    names_to = "model",
    values_to = "kendall_distance",
    names_pattern = "(.*)_") 

# renaming the factos assigning meaningful names to the levels
grid_kendall_def$sigma <- fct_recode(grid_kendall_def$sigma, "High-competitive" = "0.1", 
                                     "Average-competitive" = "1", "Low-competitive" = "10")
grid_kendall_def$model <- fct_recode(grid_kendall_def$model, "ROL" = "rol", 
                                     "Fixed-Beta" = "beta0", "Varying-Beta" = "beta1", "Dependent-Beta" = "beta2")

# save the grid with the normalised Kendall distances
saveRDS(grid_kendall_def, "grid_kendall_def.rds")


