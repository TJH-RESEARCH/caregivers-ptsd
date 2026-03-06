
////////////////////////////////////////////////////////////////////////////////
// DATA ////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

data {
  int<lower=1> N;                    // number of cases
  int<lower=1> K;                    // number of predictors - excluding P
  matrix[N, K] X;                    // matrix of predictors - centered - excluding P
  array[N] int<lower=0,upper=1> P;   // PTSD (0/1) outcome for the logistic submodel; below we center P to P_c for use as a predictor in the Gaussian models
  matrix[N, 3] Y;                    // 3 DAS standardized outcomes
  int<lower=0,upper=1> prior_only;   // switch to sample from prior only
}


////////////////////////////////////////////////////////////////////////////////
// TRANSFORMED PARAMETERS //////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

parameters {
  // PTSD (Bernoulli-logit) P | X
  real alpha_P;     // intercept in the logistic model
  vector[K] gamma;  // coeficients for logistic model predicting PTSD

  // DASS multivariate regression Y | X, P
  vector[3] alpha_Y; // intercepts for the 3 DAS outcomes
  matrix[K, 3] B;    // matrix of coefficients for each DAS outcome - excluding P
  vector[3] b_P;     // Vector of coefficients for PTSD on each DAS outcome

  // Residual covariance for Y: Sigma
  vector<lower=0>[3] sigma;                // residual SDs
  cholesky_factor_corr[3] Lcorr;  // Cholesky of correlation matrix
}


////////////////////////////////////////////////////////////////////////////////
// TRANSFORMED PARAMETERS //////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

transformed parameters {
  
  // Cholesky factor of the 3x3 Sigma
  matrix[3,3] L_Sigma;
  L_Sigma = diag_pre_multiply(sigma, Lcorr);  
  
  // Center the P variable for use as a predictor
  vector[N] P_c;
  for (i in 1:N)
    P_c[i] = P[i] - 0.5;  // Subtract 0.5 from the 0/1 dummy coding to create -.5/.5 centered coding
}


////////////////////////////////////////////////////////////////////////////////
// MODEL ///////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

model {
  
  // ** Priors ** //////////////////////////////////////////////////////////////
  // Logistic part predicting PTSD
  alpha_P ~ normal(0, 1);          // prior on intercept
  gamma ~ normal(0, 1);            // prior on coefficients - log-odds scale

  // Continuous outcomes part predicint DAS
  alpha_Y ~ normal(0, 0.5);           // outcomes are z-scored
  to_vector(B)  ~ normal(0, 0.5);     // prior on coefficients
  b_P ~ normal(0, 0.5);               // prior on coefficient for PTSD
  
  sigma   ~ normal(0, 0.7);          // half-t due to <lower=0>
  Lcorr   ~ lkj_corr_cholesky(2);  // prior on the correlation between errors


  // ** Likelihood ** //////////////////////////////////////////////////////////
  
  if (prior_only == 0) {
    // PTSD logistic regression model
    P ~ bernoulli_logit(alpha_P + X * gamma);
  
    // DAS multivariate gaussian regression model 
     {
      matrix[N,3] Mu = rep_matrix(alpha_Y', N) + X * B;
  
      for (i in 1:N) {
        Mu[i] = Mu[i] + (P_c[i] * b_P)';
        Y[i]' ~ multi_normal_cholesky(Mu[i]', L_Sigma);
      }
    }
  }
}


////////////////////////////////////////////////////////////////////////////////
// GENERATED QUANTILES /////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

generated quantities {
  
  // ** Residual covariance matrix ** //////////////////////////////////////////
  corr_matrix[3] R;
  cov_matrix[3] Sigma;
  R = multiply_lower_tri_self_transpose(Lcorr);
  Sigma = quad_form_diag(R, sigma);
  

  // ** Posterior predictive draws ** //////////////////////////////////////////
  array[N] int P_rep;
  matrix[N,3] Y_rep;
  vector[N] p_ptsd;
  
  for (i in 1:N) {
    // replicate mediator / binary outcome
    p_ptsd[i] = inv_logit(alpha_P + X[i] * gamma);
    P_rep[i]  = bernoulli_rng(p_ptsd[i]);

    // replicate Y using replicated mediator
    {
      real P_rep_c = P_rep[i] - 0.5;              // centered within Stan
      vector[3] mu_i = alpha_Y + (X[i] * B)' + (P_rep_c * b_P);
      Y_rep[i] = (multi_normal_cholesky_rng(mu_i, L_Sigma))';
    }
  }
  
  // ** Estimands ** ///////////////////////////////////////////////////////////
  vector[3] CDE_VG_P0;  // Estimate 1. VG direct effect with P fixed to 0
  vector[3] TE_VR;      // VR total effect marginalizing over P
  vector[3] TE_P;       // P total effect
  real DE_VG_P;         // VG direct effect on P
  real TE_VR_P;         // VR direct effect on P

  
  // ** Estimand 1: ////////////////////////////////////////////////////////////
  // direct effect of VG on DAS** 
  CDE_VG_P0 = (B[1])';   // VG is the first column of the predictor matrix; the coefficient equals the direct effect, no matter if P = 0 or P = 1
  
  
  // ** Estimand 2 ** //////////////////////////////////////////////////////////
  // total effect of VR on DAS** which has to be marginalized over P 
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
  
  // ** Estimand 3 ** //////////////////////////////////////////////////////////
  // total effect of PTSD on DAS, which is equal to P's coefficient on DAS
  TE_P = b_P;  
  
  // ** Estimand 4 ** //////////////////////////////////////////////////////////
  // direct effect of VG on PTSD, equal to the coefficient
  DE_VG_P = gamma[1];
  
  // ** Estimand 5 ** //////////////////////////////////////////////////////////
  // total effect of VR on PTSD, equal to the coefficient
  TE_VR_P = gamma[2];
  
}
