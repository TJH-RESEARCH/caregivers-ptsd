data {
  int<lower=0> N; // number of observations
  vector[N] V;    // vector of the outcome variables
  array[N] int<lower=0, upper=1> P;  // observed 0/1 for logistic regression
  vector[N] D;    // vector of the outcome variables
}

parameters {
  // Logistic part
  real gamma_V;
  real alpha_P;
  
  // Gaussian part
  real beta_V;
  real beta_P;
  real alpha_D;
  real<lower=0> sigma;
}

model {
  
  // Logistic Priors
  alpha_P ~ normal(0, 1.5);     // adjust based on baseline PTSD prevalence
  gamma_V ~ normal(0, 1);       // log-odds scale
  
  // Gaussian Priors
  alpha_D ~ normal(0, 5); 
  beta_V  ~ normal(0, 2);
  beta_P  ~ normal(0, 2);
  sigma ~ exponential(1);
  

  // Linear Predictors
  P ~ bernoulli_logit(alpha_P + gamma_V * V);
  D ~ normal(alpha_D + V * beta_V + to_vector(P) * beta_P, sigma);
}
