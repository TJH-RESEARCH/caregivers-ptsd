data {
  int<lower=1> N;                    // number of cases
  int<lower=1> K;                    // number of predictor
  matrix[N, K] X;                    // matrix of predictors - centered - excluding P
  array[N] int<lower=0,upper=1> P;   // PTSD (0/1) outcome for the logistic submodel
  matrix[N, 3] Y;                    // 3 DAS standardized outcomes
}

parameters {
  // PTSD (Bernoulli-logit) P | X
  real alpha_P;                      // intercept in the logistic model
  vector[K] gamma;                   // coeficients for logistic model predicting PTSD

  // DASS multivariate regression Y | X, P
  vector[3] alpha_Y;            // intercepts for the 3 DAS outcomes
  matrix[K, 3] B;               // matrix of coefficients for each DAS outcome - excluding P
  vector[3] b_P;                // Vector of coefficients for PTSD on each DAS outcome

  // Residual covariance for Y: Sigma
  vector<lower=0>[3] sigma;               // residual SDs
  cholesky_factor_corr[3] Lcorr;          // Cholesky of correlation matrix
}

transformed parameters {
  matrix[3,3] L_Sigma;
  L_Sigma = diag_pre_multiply(sigma, Lcorr); // Cholesky of Sigma
  
  // Center the P variable for use as a predictor
  vector[N] P_c;
  for (i in 1:N)
    P_c[i] = P[i] - 0.5;  // centered binary
}

model {
  // ** Priors **
  // Logistic part predicting PTSD
  alpha_P ~ normal(0, 1);          // prior on intercept
  gamma ~ normal(0, 1);            // prior on coefficients - log-odds scale

  // Continuous outcomes part predicint DAS
  alpha_Y ~ normal(0, 1);          // outcomes are z-scored
  to_vector(B)  ~ normal(0, 1);    // prior on coefficients
  b_P ~ normal(0,1);               // prior on coefficient for PTSD
  
  sigma   ~ student_t(3, 0, 1);    // half-t due to <lower=0>
  Lcorr   ~ lkj_corr_cholesky(2);

  // ** Likelihood **
  // PTSD model
  P ~ bernoulli_logit(alpha_P + X * gamma);

  // DAS model
   {
    matrix[N,3] Mu = rep_matrix(alpha_Y', N) + X * B;

    for (i in 1:N) {
      Mu[i] = Mu[i] + (P_c[i] * b_P)';
      Y[i]' ~ multi_normal_cholesky(Mu[i]', L_Sigma);
    }
  }
}

generated quantities {
  corr_matrix[3] R;
  cov_matrix[3] Sigma;
  matrix[N,3] Y_rep;
  vector[N] p_ptsd;

  // Estimands: 3 length-3 vectors 
  vector[3] CDE_VG_P0;   // Estimate 1. VG direct effect with P fixed to 0
  vector[3] CDE_VG_P1;   // Estimate 1. VG direct effect with P fixed to 1
  vector[3] TE_VR;       // VR total effect marginalizing over P
  
  vector[3] TE_P;        // P total effect
  
  real DE_VG_P;     // VG direct effect on P
  real TE_VR_P;     // VR direct effect on P

  R = multiply_lower_tri_self_transpose(Lcorr);
  Sigma = quad_form_diag(R, sigma);

  // Posterior predictive draws for Y and fitted probabilities for P
  for (i in 1:N) {
    vector[3] mu_i;
    mu_i = alpha_Y + (X[i] * B)';   // base mean excluding P
    mu_i = mu_i + (P_c[i] * b_P);    // Adds the P effect
    Y_rep[i] = (multi_normal_cholesky_rng(mu_i, L_Sigma))';
    p_ptsd[i] = inv_logit(alpha_P + X[i] * gamma);
  }
  
  // ** Estimand 1: direct effect of VG on DAS** 
  CDE_VG_P0 = (B[1])';   // VG is the first column of the predictor matrix; the coefficient equals the direct effect, no matter if P = 0 or P = 1
  CDE_VG_P1 = (B[1])';
  
  // ** Estimand 2: total effect of VR on DAS** which has to be marginalized over P 
  // VR is X col 2 coded as -0.5/+0.5. Compare do(VR=+0.5) vs do(VR=-0.5), other covariates held at observed values.
  // Marginalize over P using the mediator model:
  // E[Y|VR, X_-VR] = alpha_Y + X(VR)*B + E[P|VR, X_-VR] * b_P
  
  {
    vector[3] mu_plus_avg = rep_vector(0.0, 3);
    vector[3] mu_minus_avg = rep_vector(0.0, 3);

    for (i in 1:N) {
      row_vector[K] x_plus = X[i];
      row_vector[K] x_minus = X[i];
      real pi_plus;
      real pi_minus;

      x_plus[2] = 0.5;
      x_minus[2] = -0.5;

      pi_plus = inv_logit(alpha_P + x_plus * gamma);
      pi_minus = inv_logit(alpha_P + x_minus * gamma);

      mu_plus_avg += alpha_Y + (x_plus * B)' + (pi_plus * b_P);
      mu_minus_avg += alpha_Y + (x_minus * B)' + (pi_minus * b_P);
    }

    mu_plus_avg /= N;
    mu_minus_avg /= N;

    TE_VR = mu_plus_avg - mu_minus_avg;
  }
  
  // ** Estimand 3: total effect of PTSD on DAS ** which is equal to P's coefficient on DAS
  TE_P = b_P;  
  
  // ** Estimand 4: direct effect of VG on PTSD
  DE_VG_P = gamma[1];
  
  // ** Estimand 5: total effect of VR on PTSD
  TE_VR_P = gamma[2];
  
}
