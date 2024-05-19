data {
  // sample size:
  int <lower=0> num_obs;
  
  // number of competitors:
  int <lower=0> num_comp;  
  
  // number of games:
  int <lower=0> num_games;  
  
  // response variable: proportion of outpermerfomed  competitors:
  vector <lower=0, upper=1> [num_obs] y; 
  
  // competitor id:
  array[num_obs] int <lower=1, upper=num_comp> comp_id; 
  
  // game id: 
  array [num_obs] int <lower=1, upper= num_games> game_id; 
  
  // number of entrants for each game: 
  array[num_games] int<lower=1> n_comp_per_game;
}

parameters {
  // vector of regression coefficients associated to each competitor,
  // proxy for their abilities
  vector[num_comp] theta_driver;
  
  // fixed component of precision
  real log_phi_overall; 
  
  // vector of random components of precision
  vector[num_comp] log_phi_adj;
}

transformed parameters{
  // vector of mean parameters for each observation
  vector<lower=0,upper=1>[num_obs] mu;
  
  // vector of precision  parameters for each observation
  vector<lower=0>[num_obs] phi;
  
  // vector of the variances of abilities associated to each game
  vector<lower=0>[num_games] var_theta_game;
  
  // shape parameters
  vector<lower=0>[num_obs] alpha;   
  vector<lower=0>[num_obs] beta;
  
  // for each game the variance in the abilities of the competitors is computed
  
   { int pos = 1;
    for(k in 1:num_games) {
      int m_k = n_comp_per_game[k];
      array[m_k] int comp_idx = segment(comp_id, pos, m_k);
      vector[m_k] theta_driver_game = theta_driver[comp_idx];
      var_theta_game[k] = variance(theta_driver_game);
      pos = pos + m_k;
    }}
  
  // for each observation the mean is modeled as function of the ability of the
  //competitor and the precision is modeled as a function of the sum of the fixed 
  //component, the random component and the variance in abilities for the game
  
  for (n in 1:num_obs) {
    real theta = theta_driver[comp_id[n]];
    mu[n]=inv_logit(theta);
    phi[n]= exp(log_phi_overall + log_phi_adj[comp_id[n]]) + var_theta_game[game_id[n]];
  }
  
  // reparametrisation in terms of shape parameters 
  alpha = mu .* phi;
  beta  = (1.0 - mu) .* phi;

}

model {
  // priors
  theta_driver ~ normal(0, 1);
  log_phi_overall ~ gamma(4, 1);
  log_phi_adj ~ normal(0, 1);
  
  // likelihood
  y ~ beta(alpha, beta);
  
}




