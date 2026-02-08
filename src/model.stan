data {
  int<lower=1> N;
  vector[N] V;                        // veteran (0/1)
  array[N] int<lower=0,upper=1> P;    // for logistic style model and as a predictor in MANOVA PTSD screen (0/1)
  matrix[N, 3] Y;                     // 3 standardized outcomes for MANOVA style model
}

parameters {
  // PTSD (Bernoulli-logit) model
  real alpha_P;
  real gamma_V;

  // DASS multivariate regression
  vector[3] alpha_Y;            // intercepts for the 3 outcomes
  vector[3] beta_V;             // veteran slopes for the 3 outcomes
  vector[3] beta_P;             // PTSD slopes for the 3 outcomes

  // Residual covariance for Y: Sigma = D * R * D
  vector<lower=0>[3] sigma;               // residual SDs
  cholesky_factor_corr[3] Lcorr;          // Cholesky of correlation matrix
}

transformed parameters {
  matrix[3,3] L_Sigma;
  L_Sigma = diag_pre_multiply(sigma, Lcorr); // Cholesky of Sigma
}

model {
  // ----- Priors -----
  // Logistic part
  alpha_P ~ normal(0, 1.5);     // adjust based on baseline PTSD prevalence
  gamma_V ~ normal(0, 1);       // log-odds scale

  // Continuous outcomes part
  alpha_Y ~ normal(0, 0.5);     // outcomes are z-scored and predictors centered
  beta_V  ~ normal(0, 0.5);
  beta_P  ~ normal(0, 0.5);

  sigma   ~ student_t(3, 0, 1); // half-t due to <lower=0>
  Lcorr   ~ lkj_corr_cholesky(2);

  // ----- Likelihood -----
  // PTSD model
  P ~ bernoulli_logit(alpha_P + gamma_V * V);

  // DASS model
  for (i in 1:N) {
    vector[3] mu_i;
    mu_i = alpha_Y + beta_V * V[i] + beta_P * P[i];
    Y[i]' ~ multi_normal_cholesky(mu_i, L_Sigma);
  }
}

generated quantities {
  corr_matrix[3] R;
  cov_matrix[3] Sigma;
  matrix[N,3] Y_rep;
  vector[N] p_ptsd;

  R = multiply_lower_tri_self_transpose(Lcorr);
  Sigma = quad_form_diag(R, sigma);

  for (i in 1:N) {
    vector[3] mu_i;
    mu_i = alpha_Y + beta_V * V[i] + beta_P * P[i];

    // posterior predictive for Y
    Y_rep[i] = (multi_normal_cholesky_rng(mu_i, L_Sigma))';

    // predicted PTSD probability
    p_ptsd[i] = inv_logit(alpha_P + gamma_V * V[i]);
  }
}
