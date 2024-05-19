data {
  // sample size:
  int <lower=0> num_obs; 
  
  // number of competitors:
  int <lower=0> num_comp; 
  
  // response variable: proportion of outpermerfomed  competitors:
  vector <lower=0, upper=1> [num_obs] y;
  
  // competitor id:
  array[num_obs] int <lower=1, upper=num_comp> comp_id;
}

parameters {
  // vector of regression coefficients associated to each competitor,
  // proxy for their abilities:
  vector[num_comp] theta_driver;
  
  // fixed component of precision
  real<lower=0> phi_overall; 
  
   // vector of random components of precision:
  vector[num_comp] phi_adj;
}

transformed parameters{
   // vector of mean parameters for each observation:
  vector<lower=0,upper=1>[num_obs] mu;
  
  // vector of precision  parameters for each observation:
  vector<lower=0>[num_obs] phi;
  
  // shape parameters
  vector<lower=0>[num_obs] alpha;   
  vector<lower=0>[num_obs] beta;
  
  // for each observation the mean is modeled as function of the ability of the
  //competitor and the precision is modeled as a function of the sum of the fixed 
  //component and  the random component
   for (n in 1:num_obs) {
    real theta = theta_driver[comp_id[n]];
    mu[n]=inv_logit(theta);
    phi[n]= exp(phi_overall + phi_adj[comp_id[n]]);
  }
  
  // reparameterisation in terms of shape parameters 
  alpha = mu .* phi;
  beta  = (1.0 - mu) .* phi;
}

model {
  // priors
  theta_driver ~ normal(0, 1);
  phi_overall ~ gamma(4, 1);
  phi_adj ~ normal(0, 1);
  
  // likelihood
  y ~ beta(alpha, beta);
}
