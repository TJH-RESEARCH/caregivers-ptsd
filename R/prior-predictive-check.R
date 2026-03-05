

# Fit the priors only ----------------------------------------------------------
fit_prior <- 
  model$sample(
    data = stan_data_prior,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 2000,
    iter_sampling = 6000,
    seed = 30144
  )

# Get draws
draws_pred <- fit_prior$draws(c("P_rep", "Y_rep"))
draws_pred <- as_draws_df(draws_pred)



# Prior predictive prevalence of PTSD ------------------------------------------
p_pred <- 
  draws_pred %>%
  transmute(.draw, pred = rowMeans(across(starts_with("P_rep["))))

# Plot
ggplot(p_pred, aes(x = pred)) + geom_histogram(bins = 40)

# Save plot to file
ggsave(plot = plot_prior_y, filename = here("output/plot-prior-y.jpg"), height = 4, width = 6)



# Prior predictive distribution of Y (pooled) ----------------------------------
y_long <- 
  draws_pred %>% 
  dplyr::select(starts_with("Y_rep[")) |>
  pivot_longer(everything(), values_to = "y")

# Plot
plot_prior_y <- ggplot(y_long, aes(x = y)) + geom_density()

# Save plot to file
ggsave(plot = plot_prior_y, filename = here("output/plot-prior-y.jpg"), height = 4, width = 6)

