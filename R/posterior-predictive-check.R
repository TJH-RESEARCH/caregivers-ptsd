
# draws as an array: iterations x chains x variables
ydraws <- fit$draws("Y_rep")

# dims: iter x chain x (flattened variables)
dim(ydraws)

draws_mat <- as_draws_matrix(ydraws)  # rows = posterior draws, cols = variables

# Identify columns belonging to Y_rep
ycols <- grep("^Y_rep\\[", colnames(draws_mat))
Yrep_mat <- draws_mat[, ycols, drop = FALSE]  # S x (N*3)

S <- nrow(Yrep_mat)
N <- nrow(data_sim) # observed N

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

set.seed(1)
draw_ids <- sample.int(S, size = min(50, S))

Y <- data_sim %>% dplyr::select(dass_stress_z, dass_anxiety_z, dass_depression_z)
y_rep_long_stress <- 
  as_tibble(Yrep[draw_ids, , 1]) %>%
  mutate(draw = row_number()) %>%
  pivot_longer(-draw, values_to = "y") %>%
  dplyr::select(draw, y)

y_rep_long_anxiety <- 
  as_tibble(Yrep[draw_ids, , 2]) %>%
  mutate(draw = row_number()) %>%
  pivot_longer(-draw, values_to = "y") %>%
  dplyr::select(draw, y)

y_rep_long_depression <- 
  as_tibble(Yrep[draw_ids, , 3]) %>%
  mutate(draw = row_number()) %>%
  pivot_longer(-draw, values_to = "y") %>%
  dplyr::select(draw, y)

ggplot() +
  geom_density(data = y_rep_long_stress, aes(x = y, group = draw), alpha = 0.01, color = colors_johnson[5]) +
  geom_density(data = Y, aes(x = dass_stress_z), linewidth = 1, color = colors_johnson[3]) +
  labs(title = "PPC: Stress",
       x = "value", y = "density") +
  theme_minimal()

ggplot() +
  geom_density(data = y_rep_long_anxiety, aes(x = y, group = draw), alpha = 0.01, color = colors_johnson[5]) +
  geom_density(data = Y, aes(x = dass_anxiety_z), linewidth = 1, color = colors_johnson[3]) +
  labs(title = "PPC: Anxiety",
       x = "value", y = "density") +
  theme_minimal()

ggplot() +
  geom_density(data = y_rep_long_depression, aes(x = y, group = draw), alpha = 0.01, color = colors_johnson[5]) +
  geom_density(data = Y, aes(x = dass_depression_z), linewidth = 1, color = colors_johnson[3]) +
  labs(title = "PPC: Depression",
       x = "value", y = "density") +
  theme_minimal()
