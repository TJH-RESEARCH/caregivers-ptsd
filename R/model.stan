data {
  int<lower=1> N;                    // number of cases
  int<lower=1> K;                    // number of predictor
  matrix[N, K] X;                    // matrix of predictors - centered
  array[N] int<lower=0,upper=1> P;   // PTSD (0/1) outcome for the logistic submodel 
  matrix[N, 3] Y;                    // 3 DAS standardized outcomes
}

parameters {
  // PTSD (Bernoulli-logit) model
  real alpha_P;                      // intercept in the logistic model
  vector[K] gamma;                   // coeficients for logistic model predicting PTSD

  // DASS multivariate regression
  vector[3] alpha_Y;            // intercepts for the 3 DAS outcomes
  matrix[K, 3] B;               // matrix of coefficients for each DAS outcome 

  // Residual covariance for Y: Sigma
  vector<lower=0>[3] sigma;               // residual SDs
  cholesky_factor_corr[3] Lcorr;          // Cholesky of correlation matrix
}

transformed parameters {
  matrix[3,3] L_Sigma;
  L_Sigma = diag_pre_multiply(sigma, Lcorr); // Cholesky of Sigma
}

model {
  // ----- Priors -----
  // Logistic part predicting PTSD
  alpha_P ~ normal(0, 1);          // prior on intercept
  gamma ~ normal(0, 1);            // prior on coefficients - log-odds scale

  // Continuous outcomes part predicint DAS
  alpha_Y ~ normal(0, 1);          // outcomes are z-scored
  to_vector(B)  ~ normal(0, 1);    // prior on coefficients
  
  sigma   ~ student_t(3, 0, 1);    // half-t due to <lower=0>
  Lcorr   ~ lkj_corr_cholesky(2);

  // ----- Likelihood -----
  // PTSD model
  P ~ bernoulli_logit(alpha_P + X * gamma);

  // DAS model
   {
    matrix[N,3] Mu = rep_matrix(alpha_Y', N) + X * B;
    //matrix[3,3] L_Sigma = diag_pre_multiply(sigma, Lcorr);

    for (i in 1:N)
      Y[i]' ~ multi_normal_cholesky(Mu[i]', L_Sigma);
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

    // X[i] * B is row_vector[3], then transpose
    mu_i = alpha_Y + (X[i] * B)';

    Y_rep[i] = (multi_normal_cholesky_rng(mu_i, L_Sigma))';

    p_ptsd[i] = inv_logit(alpha_P + X[i] * gamma);
  }
}
