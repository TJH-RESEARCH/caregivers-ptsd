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


# Create dummy variables
data <-
  data %>% 
  mutate(
    education_less_hs = ifelse(education == "Less than high school degree", 1, 0),
    education_hs = ifelse(education == "High school diploma or equivalent", 1, 0),
    education_some_college = ifelse(education == "Some college", 1, 0),
    education_associates = ifelse(education == "Associate's degree", 1, 0),
    education_bachelors = ifelse(education == "Bachelor's degree", 1, 0),
    education_grad = ifelse(education == "Master's degree" | 
                              education == "Professional doctorate" | 
                              education == "Doctoral degree", 1, 0)
  )

data <-
  data %>%
  mutate(
    income_less_10k = ifelse(income == "< $10,000", 1, 0),
    income_10k_19k = ifelse(income == "$10,000 - $19,999", 1, 0),
    income_20k_49k = ifelse(income == "$20,000 - $49,999", 1, 0),
    income_50k_99k = ifelse(income == "$50,000 - $99,999", 1, 0),
    income_100k_149k = ifelse(income == "$100,000 - $149,999", 1, 0),
    income_150k = ifelse(income == "> $150,000", 1, 0),
  )




# Create a matrix of predictor variables - excluding PTSD
X <- 
  data %>% 
  
  # center binary variable by subtracting 0.5
  transmute(
    VG = recipient_veteran - 0.5,
    VR = caregiver_veteran - 0.5,
    
    # Race/Ethnicity: No one was in the HPI group; no reference as they were not mutually exclusive
    Rb = race_black - 0.5,
    Rw = race_white - 0.5,
    Ra = race_asian - 0.5,
    Rl = race_latinx - 0.5,
    Rm = race_mena - 0.5,
    Rn = race_native - 0.5,
    Ro = race_other - 0.5,
    
    # Gender: Reference male
    Gf = gender_female - 0.5,
    
    # Education: Reference less than high school
    Eh = education_hs - 0.5,
    Es = education_some_college - 0.5,
    Ea = education_associates - 0.5,
    Eb = education_bachelors - 0.5,
    Eg = education_grad - 0.5,
    
    # Income: Reference less than 10k
    Il = income_10k_19k - 0.5,
    It = income_20k_49k - 0.5, 
    If = income_50k_99k - 0.5, 
    Ih = income_100k_149k - 0.5, 
    Iu = income_150k - 0.5, 
    
    # Age
    A = age_z,
  ) %>% as.matrix()


# Gather the variables for the stan model
stan_data <- 
  list(
    N = nrow(data),
    X = X,
    K = ncol(X),
    P = data$pc_ptsd_positive_screen,
    Y = as.matrix(
      data[, c("dass_depression_z", 
                   "dass_anxiety_z",
                   "dass_stress_z"
      )]),
    prior_only = 0L # This is a flag that the stan code uses to sample from the prior only or not
  )


# Create prior only data
stan_data_prior <- within(stan_data, prior_only <- 1L)