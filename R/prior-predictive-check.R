

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
(plot_prior_p <- 
  p_pred %>% 
  ggplot(aes(x = pred)) + 
  geom_histogram(bins = 40, 
                 fill = colors_johnson[1],
                 alpha = .9,
                 color = "black", 
                 linewidth = .2) +
  labs(
     x = "Prior Prediction (probability)",
     y = "Count",
     title = "Prior Predictive Distribution",
     subtitle = "Probable PTSD Detection") + 
  theme_density
)

# print plot to window
plot_prior_p %>% print()

# Save plot to file
ggsave(plot = plot_prior_p, filename = here("output/plot-prior-p.jpg"), height = 4, width = 6)



# Prior predictive distribution of Y (pooled) ----------------------------------
y_long <- 
  draws_pred %>% 
  dplyr::select(starts_with("Y_rep[")) |>
  pivot_longer(everything(), values_to = "Y_pred")

# Plot
(plot_prior_y <- 
  y_long %>% 
  ggplot(aes(x = Y_pred)) + 
  geom_density(fill = colors_johnson[5], alpha = .5) + 
  labs(x = "Prior Prediction (z score)",
       y = NULL,
       title = "Prior Predictive Distribution",
       subtitle = "Depression, Anxiety, and Stress (Pooled Outcomes)") + 
  theme_density)

# print plot to window
plot_prior_y %>% print()

# Save plot to file
ggsave(plot = plot_prior_y, filename = here("output/plot-prior-y.jpg"), height = 4, width = 6)

