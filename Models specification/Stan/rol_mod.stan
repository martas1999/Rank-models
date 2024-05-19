// hierarchical rank-ordered logit regression in stan
functions {
  // log-likelihood; see http://www.glicko.net/research/multicompetitor.pdf
  real rank_ordered_logit(vector ordered_skills, int Mk) {
    real ll = 0;
    for (m in 1:(Mk - 1)) {
      ll += ordered_skills[m] - log_sum_exp(ordered_skills[m:Mk]);
    }
    return ll;
  }
}

data {
  int <lower=0> num_obs; // sample size
  int <lower=0> num_comp; // number of competitors
  int <lower=0> num_games; // number of games
  
  array[num_obs] int <lower=1, upper=num_comp> ranked_comp_ids; // competitor ranking for each race
  array[num_games] int<lower=1> n_comp_per_game; // number of entrants for each game
}

parameters {
   vector[num_comp] theta;
}

model {
  theta ~ normal(0, 1);
  int pos =1 ;
  for(k in 1:num_games){
    int m_k = n_comp_per_game[k];
    array[m_k] int comp_idx = segment(ranked_comp_ids, pos, m_k);
    vector[m_k] comp_skills = theta[comp_idx];
    
    target += rank_ordered_logit(comp_skills,m_k);
    pos = pos + m_k;
  }
}
