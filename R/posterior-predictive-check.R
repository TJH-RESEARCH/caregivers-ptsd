
# Get the Y_rep columns and rename them to be descriptive:

draws_y_rep <- draws %>% select(contains('Y_rep'))

replacements_y_rep <- c(
  "\\,1\\]" = "_D", # Note the 1,2,3 order depends on the order of DAS in the data supplied to Stan
  "\\,2\\]" = "_A",
  "\\,3\\]" = "_S",
  "\\," = "_",
  "\\[" = "_",
  "\\]" = "")

names(draws_y_rep) <- str_replace_all(names(draws_y_rep), replacements_y_rep)

# Add posterior draw index
draws_y_rep <- draws_y_rep %>%
  mutate(draw = row_number())

# Convert to long format
y_rep_long <- 
  draws_y_rep %>%
  pivot_longer(
    cols = -draw,
    names_to = "name",
    values_to = "y"
  ) %>%
  extract(
    name,
    into = c("prefix", "id", "outcome"),
    regex = "^(Y_rep)_([0-9]+)_([DAS])$",
    remove = TRUE
  ) %>%
  mutate(
    id = as.integer(id),
    outcome = recode(outcome,
                            D = "depression",
                            A = "anxiety",
                            S = "stress"
    )
  ) %>%
  select(draw, id, outcome, y)


# Sample some posterior draws
set.seed(30314)

draw_ids <- 
  y_rep_long %>%
  distinct(draw) %>%
  slice_sample(n = min(50, nrow(.))) %>%
  pull(draw)

y_rep_plot <- 
  y_rep_long %>%
  filter(draw %in% draw_ids)

# Make an observed dataset in matching long format
y_obs_long <- 
  data %>%
  mutate(id = row_number()) %>%
  select(
    id,
    depression = dass_depression_z,
    anxiety = dass_anxiety_z,
    stress = dass_stress_z
  ) %>%
  pivot_longer(
    cols = -id,
    names_to = "outcome",
    values_to = "y"
  )


# Plot --------------------------------------------------------------------
plot_posterior_pred <-
  ggplot() +
  geom_density(
    data = y_rep_plot,
    aes(x = y, group = draw),
    alpha = 0.01,
    color = colors_johnson[5]
  ) +
  geom_density(
    data = y_obs_long,
    aes(x = y),
    linewidth = 1,
    color = colors_johnson[3]
  ) +
  facet_wrap(~ outcome, scales = "free") +
  labs(
    title = "Posterior Predictive Distribution",
    x = "Prediction",
    y = NULL
  ) +
  theme_density

## Print to window
plot_posterior_pred %>% plot()

## Save to console
ggsave(plot = plot_posterior_pred, here("output/posterior-pred.jpg"), height = 4, width = 6)
