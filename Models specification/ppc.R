# needed libraries
library(ggplot2)
library(tidyverse)

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

# example dataset with 50 games, 20 total competitors, 7 entrants and average-competitiveness (sigma = 1) 
dat_list <- simulate_data(50, 20, 7, 1)
df_tidy <- prep_data(dat_list)

# how the proportion of competitors beaten for each competitors looks like for a given dataset 
 
# histograms
df_tidy %>%
  group_by(comp_id) %>%
  ggplot() +
  geom_histogram(aes(x = prop_beaten_transformed)) +
  facet_wrap(~comp_id) 

# density plots
df_tidy %>%
  group_by(comp_id) %>%
  ggplot() +
  geom_density(aes(x = prop_beaten_transformed)) +
  facet_wrap(~comp_id) 



#' Inverse logit function
#' @param x a number
#' 
#' @returns the inverse logit of x
invlogit <- function(x) exp(x)/ (1 + exp(x))

## PRIOR PREDICTIVE CHECKS

## Prior predictive check for the Fixed-Beta model

## Simulation of proportions of competitors beaten per competitor as Beta 
# distributed variables with FIXED precision parameter phi

# Prior distribution for phi = Gamma(4,1)
dev.off()
for (i in 1:50) {
  theta <- rnorm(1, 0, 1)
  mu    <- invlogit(theta)
  phi <- rgamma(1, 4, 1)
  alpha <- mu * phi
  beta  <- (1-mu) * phi
  curve(dbeta(x, alpha, beta), from=0, to=1, ylim=c(0, 4), add = TRUE)
}
# remember to clear the plots before running the next code chunk
# Prior distribution for phi = Gamma(1,1)
for (i in 1:50) {
  theta <- rnorm(1, 0, 1)
  mu    <- invlogit(theta)
  phi <- rgamma(1, 1, 1)
  alpha <- mu * phi
  beta  <- (1-mu) * phi
  curve(dbeta(x, alpha, beta), from=0, to=1, ylim=c(0, 5), add = TRUE)
}

# remember to clear the plots before running the next code chunk
# Prior distribution for phi = Gamma(1,4)
for (i in 1:50) {
  theta <- rnorm(1, 0, 1)
  mu    <- invlogit(theta)
  phi <- rgamma(1, 1, 4)
  alpha <- mu * phi
  beta  <- (1-mu) * phi
  curve(dbeta(x, alpha, beta), from=0, to=1, ylim=c(0, 5), add = TRUE)
}

# remember to clear the plots before running the next code chunk
# Prior distribution for phi = Normal(0,1)
for (i in 1:50) {
  theta <- rnorm(1, 0, 1)
  mu    <- invlogit(theta)
  phi <- rnorm(1, 0, 1)
  alpha <- mu * phi
  beta  <- (1-mu) * phi
  curve(dbeta(x, alpha, beta), from=0, to=1, ylim=c(0, 5),add = TRUE)
}

# remember to clear the plots before running the next code chunk
#  Prior distribution for phi = lognormal(0,1)
for (i in 1:50) {
  theta <- rnorm(1, 0, 1)
  mu    <- invlogit(theta)
  phi   <- rlnorm(1, meanlog = 0, sdlog = 1)
  alpha <- mu * phi
  beta  <- (1-mu) * phi
  curve(dbeta(x, alpha, beta), from=0, to=1, ylim=c(0, 5), add = TRUE)
}
  
# remember to clear the plots before running the next code chunk 
#  Prior distribution for phi = Weibull(2,1)
for (i in 1:50) {
  theta <- rcauchy(1, 0, 2.5)
  mu    <- invlogit(theta)
  phi   <- rweibull(1, shape = 2, scale = 1)
  alpha <- mu * phi
  beta  <- (1-mu) * phi
  curve(dbeta(x, alpha, beta), from=0, to=1, ylim=c(0, 5), add = TRUE)
}

# remember to clear the plots before running the next code chunk
#  Prior distribution for phi = tstudent(1,1)
for (i in 1:50) {
  theta <- rcauchy(1, 0, 2.5)
  mu    <- invlogit(theta)
  phi   <- rt(1, df = 1)
  alpha <- mu * phi
  beta  <- (1-mu) * phi
  curve(dbeta(x, alpha, beta), from=0, to=1, ylim=c(0, 10), add = TRUE)
} 

## Gamma(4,1) seems to be the most appropriate prior for phi


## PRIOR PREDICTIVE CHECK FOR Varying-Beta model
for (i in 1:50) {
  theta <- rnorm(1, 0, 1)
  phi_overall <- rgamma(1, 4, 1)
  phi_adj <-  rnorm(1, 0, 1)
  mu    <- invlogit(theta)
  phi <- exp(phi_overall + phi_adj)
  alpha <- mu * phi
  beta  <- (1-mu) * phi
  curve(dbeta(x, alpha, beta), from=0, to=1, ylim=c(0, 10), add = TRUE)
}

## PRIOR PREDICTIVE CHECK FOR Dependent-Beta model
for (i in 1:50) {
  theta <- rnorm(1, 0, 1)
  thetas <- c(theta, rnorm(7, 0, 1))
  phi_overall <- rgamma(1, 4, 1)
  phi_adj <-  rnorm(1, 0, 1)
  mu    <- invlogit(theta)
  phi <- exp(phi_overall + phi_adj+ (var(thetas)))
  alpha <- mu * phi
  beta  <- (1-mu) * phi
  curve(dbeta(x, alpha, beta), from=0, to=1, ylim=c(0, 10), add = TRUE)
}






## -----------------------------------------------------------------------------
# A posterior predictive check is the comparison between what the fitted model 
# predicts and the actual observed data. The aim is to detect if the model is 
# inadequate to describe the data. The idea behind posterior predictive checking 
# is simple: if a model is a good fit then we should be able to use it to generate 
# data that looks a lot like the data we observed. posterior predictive distribution.
# This is the distribution of the outcome variable implied by a model after using 
# the observed data 𝑦 (a vector of 𝑁 outcome values) to update our beliefs about
# unknown model parameters 𝜃. The posterior predictive distribution for observation 
# 𝑦˜ can be written as
?tidy_dpp()
## POSTERIOR PREDICTIVE CHECKS
df_tidy <- read_rds("example_data.rds")

beta0_post_check <- cmdstan_model("stanfiles/beta0_post_check.stan")

beta0_post_check_dat <-list(
  num_obs   = nrow(df_tidy),
  num_comp  = 15,
  num_games = length(unique(df_tidy$game_id)),
  comp_id   = df_tidy$comp_id,
  y         = df_tidy$prop_beaten_transformed
)

fit_beta0_post_check <- beta0_post_check$sample(
  data = beta0_post_check_dat,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)


library(posterior)

fit_beta0_post_check_draws <- fit_beta0_post_check$draws(format = "df")
ncol(fit_beta0_post_check_draws)
tail(names(fit_beta0_post_check_draws)) 
y_simulated <- fit_beta0_post_check_draws[, 1258:1267]
y_sim_means <- colMeans(y_simulated)

hist(y_sim_means)

boh <- df_tidy %>% 
  filter(comp_id == 1) 
  
hist(boh$prop_beaten_transformed)



## -----------------------------------------------------------------------------


tentgamma <- rgamma(1000, 4, 1)
tentcauchy <- rcauchy(1000, 0, 2.5)
tentnorm <- rnorm(1000, 0, 1)

hist(tentnorm)

curve(dgamma(x, 4, 1), from=0, to=10, ylim=c(0, 0.5))
curve(dcauchy(x, 0, 2.5), from=-10, to=10, add = TRUE)
curve(dnorm(x, 0, 1), from=-5, to=5, add = TRUE)

tentinv <- exp(tentcauchy)/(1+exp(tentcauchy))

hist(tentinv)
