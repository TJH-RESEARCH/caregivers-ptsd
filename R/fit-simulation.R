# Create a matrix of predictor variables
X_sim <- 
  data_sim %>% 
  # center by subtracting 0.5
  transmute(
    VG = recipient_veteran - 0.5,
    VR = caregiver_veteran - 0.5,
    Rw = race_white - 0.5,
    Rb = race_black - 0.5,
    Gf = gender_female - 0.5,
    Eh = education_higher - 0.5,
    El = education_lower - 0.5,
    Ih = income_higher - 0.5,
    Il = income_lower - 0.5, 
    A = age,
  ) %>% as.matrix()


# Gather the variables for the stan model
stan_data_sim <- 
  list(
    N = nrow(data_sim),
    X = X_sim,
    K = ncol(X_sim),
    P = data_sim$pc_ptsd_positive_screen,
    Y = as.matrix(
      data_sim[, c("dass_depression_z", 
                   "dass_anxiety_z",
                   "dass_stress_z"
      )]),
    prior_only = 0L
  )

# Fit the model
fit_sim <- 
  model$sample(
    data = stan_data_sim,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 2000,
    iter_sampling = 6000,
    seed = 30144
  )
