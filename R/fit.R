
# Fit the model
fit <- 
  model$sample(
    data = stan_data,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 2000,
    iter_sampling = 6000,
    seed = 30144
  )

# Estract posterior draws
draws <- fit$draws() %>% as_draws_df() %>% tibble()

# Just the estimands
draws_estimands <-
  draws %>% 
  dplyr::select(.chain, .iteration, .draw, starts_with("TE"), starts_with('CDE'), starts_with("DE"))

## just the residual variance and covariance
draws_variance <- 
  draws %>% 
  dplyr::select(.chain, .iteration, .draw, 
                starts_with("sigma"), 
                starts_with("Lcorr"), 
                starts_with("L_Sigma"))

# Fix names  
replacements <- c(
  "1\\,\\]" = "_D", # Note the 1,2,3 order depends on the order of DAS in the data supplied to Stan
  "2\\,\\]" = "_A",
  "3\\,\\]" = "_S",
  "1" = "D",
  "2" = "A", 
  "3" = "S",
  "\\," = "_",
  "\\[" = "_",
  "\\]" = "")
names(draws_variance) <- str_replace_all(names(draws_variance), replacements)
names(draws_estimands) <- str_replace_all(names(draws_estimands), replacements)

