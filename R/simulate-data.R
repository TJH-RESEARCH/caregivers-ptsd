# Set RNG seed
set.seed(30144)

# Set sample size for simulated data set
n <- 300

# regression coefficients (one per outcome)
beta_VG <- c(d = 0.5, s = 0.3, a = 0.2)
beta_VR <- c(d = 0.5, s = 0.3, a = 0.2)
beta_PTSD <- c(d = 0.7, s = 0.7, a = 0.7)

# residual covariance
Sigma <- matrix(
  c(1.0, 0.6, 0.5,
    0.6, 1.0, 0.4,
    0.5, 0.4, 1.0),
  nrow = 3
)

# correlated errors
E <- MASS::mvrnorm(n, mu = c(0, 0, 0), Sigma = Sigma)

## Data
data_sim <- 
  tibble(
    mil_family = rbinom(n, size = 1, prob = .4),
    VG = ifelse(mil_family == 1, 
                rbinom(n, size = 1, prob = .5), 
                rbinom(n, size = 1, prob = .2)),
    VR = ifelse(mil_family == 1, 
                rbinom(n, size = 1, prob = .5), 
                rbinom(n, size = 1, prob = .25))
  ) %>% 
  rowwise() %>% 
  mutate(pc_ptsd_positive_screen = 
           rbinom(n = 1, size = 1, 
                  prob = plogis(VR * .2 + VG * .2 + rnorm(1, 0, .2)))) %>% 
  ungroup() %>% 
  mutate(
    dass_depression_z = 
      beta_VR["d"] * VR + 
      beta_VG["d"] * VG + 
      beta_PTSD["d"] * pc_ptsd_positive_screen + 
      E[,1],
    dass_stress_z = 
      beta_VR["s"] * 
      VR + beta_VG["s"] * 
      beta_PTSD["s"] * pc_ptsd_positive_screen + 
      VG + E[,2],
    dass_anxiety_z = 
      beta_VR["a"] * VR + 
      beta_VG["a"] * VG + 
      beta_PTSD["a"] * pc_ptsd_positive_screen + 
      E[,3]) %>% 
  rename(caregiver_veteran = VG, 
         recipient_veteran = VR) %>% 
  
  # Add demographic covariates...without coefficients, just to see the model run
  rowwise() %>% 
  mutate(race = sample(c('white', 'Black', 'other'), size = 1),
         gender_female = rbinom(1, 1, prob = .5),
         education = sample(c("lower", "average", "higher"), size = 1),
         income = sample(c("lower", "average", "higher"), size = 1),
         age = rnorm(n = 1, mean = 0, sd = 1)
  ) %>% 
  ungroup() %>% 
  mutate(race_black = ifelse(race == 'Black', 1, 0),
         race_white = ifelse(race == 'white', 1, 0),
         education_higher = ifelse(education == "higher", 1, 0),
         education_lower = ifelse(education == "lower", 1, 0),
         income_higher = ifelse(income == "higher", 1, 0),
         income_lower = ifelse(income == "lower", 1, 0)
  )

