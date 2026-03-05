
# Posterior Variance
## Plot the posterior variance from the model of simulated data. 
draws_variance %>% 
  dplyr::select(sigma_S, sigma_A, sigma_D) %>%
  pivot_longer(everything()) %>% 
  ggplot(aes(value, fill = name)) +
  geom_density(alpha = .5)


# Posterior Covariance
## Plot the posterior residual covariance from the model of simulated data. 
draws_variance %>% 
  dplyr::select(Sigma_A_D, Sigma_A_S, Sigma_D_S) %>%
  pivot_longer(everything()) %>% 
  ggplot(aes(value, fill = name)) +
  geom_density(alpha = .5)


# Fix names
names(draws_estimands) <- str_replace_all(names(draws_estimands), replacements)
draws_estimands %>% posterior::summarise_draws()


# H1: Depression, Anxiety, and Stress will be worse for caregivers who are veterans.
## We estimated the direct effect.
draws_estimands %>% 
  dplyr::select(starts_with("CDE_VG")) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(value, fill = name)) + 
  geom_density(alpha = .5)


# H2: Depression, Anxiety, and Stress will be worse for caregivers to veterans than caregivers to non-veterans.
## We estimated the total effect.
draws_estimands %>% 
  dplyr::select(starts_with("TE_VR") & !TE_VR_P) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(value, fill = name)) + 
  geom_density(alpha = .5)


# H3: Depression, Anxiety, and Stress will be worse for caregivers with probable PTSD.
draws_estimands %>% 
  dplyr::select(starts_with("TE_P")) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(value, fill = name)) + 
  geom_density(alpha = .5)


# H4: Probable PTSD will be more prevalent in veteran caregivers.
draws_estimands %>% 
  dplyr::select(starts_with("DE_VG_P")) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(value, fill = name)) + 
  geom_density(alpha = .5)


# H5: Probable PTSD will be more prevalent in caregivers to veterans than caregivers to non-veterans.
draws_estimands %>% 
  dplyr::select(starts_with("TE_VR_P")) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(value, fill = name)) + 
  geom_density(alpha = .5)


