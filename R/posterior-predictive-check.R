
# draws as an array: iterations x chains x variables
ydraws <- fit$draws("Y_rep")

draws_mat <- as_draws_matrix(ydraws)  # rows = posterior draws, cols = variables

# Identify columns belonging to Y_rep
ycols <- grep("^Y_rep\\[", colnames(draws_mat))
Yrep_mat <- draws_mat[, ycols, drop = FALSE]  # S x (N*3)

S <- nrow(Yrep_mat)
N <- nrow(data) # observed N

# Reshape to S x N x 3
Yrep <- array(NA_real_, dim = c(S, N, 3))
for (k in 1:3) {
  # Stan indexes Y_rep[i,k]. Columns are in that order in draws_mat.
  cols_k <- grep(paste0("^Y_rep\\[\\d+,", k, "\\]$"), colnames(Yrep_mat))
  Yrep[,,k] <- as.matrix(Yrep_mat[, cols_k, drop = FALSE])
}

# Name outcomes to avoid confusion
outcome_names <- c("stress","anxiety","depression")
dimnames(Yrep) <- list(draw = NULL, id = NULL, outcome = outcome_names)

set.seed(30314)
draw_ids <- sample.int(S, size = min(50, S))

Y <- data %>% select(dass_stress_z, dass_anxiety_z, dass_depression_z)


y_rep_depression <- 
  as_tibble(Yrep[draw_ids, , 1]) %>% # Depression is the first column
  mutate(draw = row_number()) %>%
  pivot_longer(-draw, values_to = "y") %>%
  dplyr::select(draw, y)

y_rep_anxiety <- 
  as_tibble(Yrep[draw_ids, , 2]) %>% # Anxiety is the second column
  mutate(draw = row_number()) %>%
  pivot_longer(-draw, values_to = "y") %>%
  dplyr::select(draw, y)

y_rep_stress <- 
  as_tibble(Yrep[draw_ids, , 3]) %>%  # Stress is the third column
  mutate(draw = row_number()) %>%
  pivot_longer(-draw, values_to = "y") %>%
  dplyr::select(draw, y)


# Depression
posterior_pred_depression <-
  ggplot() +
  # predicted data:
  geom_density(data = y_rep_depression, aes(x = y, group = draw), alpha = 0.01, color = colors_johnson[5]) +
  # observed data:
  geom_density(data = data, aes(x = dass_depression_z), linewidth = 1, color = colors_johnson[3]) +
  labs(title = "Posterior Predictive Distribution",
       subtitle = "Depression",
       x = "Prediction",
       y = NULL) +
  theme_density

## Print to window
posterior_pred_depression %>% plot()

## Save to file
ggsave(plot = posterior_pred_depression, here("output/posterior-pred-depression.jpg"))


# Anxiety
posterior_pred_anxiety <-
ggplot() +
  geom_density(data = y_rep_anxiety, aes(x = y, group = draw), alpha = 0.01, color = colors_johnson[5]) +
  geom_density(data = data, aes(x = dass_anxiety_z), linewidth = 1, color = colors_johnson[3]) +
  labs(title = "Posterior Predictive Distribution",
       subtitle = "Anxiety",
       x = "Prediction",
       y = NULL) +
  theme_density

## Print to window
posterior_pred_anxiety %>% plot()

## Save to console
ggsave(plot = posterior_pred_anxiety, here("output/posterior-pred-anxiety.jpg"))


# Stress
posterior_pred_stress <-
  ggplot() +
  geom_density(data = y_rep_stress, aes(x = y, group = draw), alpha = 0.01, color = colors_johnson[5]) +
  geom_density(data = data, aes(x = dass_stress_z), linewidth = 1, color = colors_johnson[3]) +
  labs(title = "Posterior Predictive Distribution",
       subtitle = "Stress",
       x = "Prediction",
       y = NULL) +
  theme_density


## Print to window
posterior_pred_stress %>% plot()

## Save to console
ggsave(plot = posterior_pred_stress, here("output/posterior-pred-stress.jpg"))
