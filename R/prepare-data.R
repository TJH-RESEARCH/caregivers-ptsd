# Prepare the data for modeling

# Standardize continuous variables
data <-
  data %>% 
  mutate(
    dass_depression_z = as.numeric(scale(dass_depression)),
    dass_anxiety_z = as.numeric(scale(dass_anxiety)),
    dass_stress_z = as.numeric(scale(dass_stress)),
    age_z = as.numeric(scale(age))
  )


# Center binary variables
data <- data %>% mutate()


# Create a matrix of predictor variables
X <- 
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
stan_data <- 
  list(
    N = nrow(data_sim),
    X = X,
    K = ncol(X),
    P = data_sim$pc_ptsd_positive_screen,
    Y = as.matrix(
      data_sim[, c("dass_depression_z", 
                   "dass_anxiety_z",
                   "dass_stress_z"
      )]),
    prior_only = 0L # This is a flag that the stan code uses to sample from the prior only or not
  )


# Create prior only data
stan_data_prior <- within(stan_data, prior_only <- 1L)