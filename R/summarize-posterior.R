# Contrast in means
## Given these are binary variables, the coefficients are equal to the contrast in means.
library(bayestestR) # For summarizing the posterior

draws_estimands %>% posterior::summarise_draws()
draws_variance %>% posterior::summarise_draws()

draws_estimands %>%
  posterior::summarise_draws() |>
  mutate(
    outcome = case_when(
      str_detect(variable, "_D") ~ "Depression",
      str_detect(variable, "_A") ~ "Anxiety",
      str_detect(variable, "_S") ~ "Stress",
      str_detect(variable, "_P") ~ "PTSD",
      .default = NA
    ),
    effect = case_when(
      str_detect(variable, "CDE") ~ "Conditional Direct",
      str_detect(variable, "DE") ~ "Direct",
      str_detect(variable, "TE") ~ "Total",
      .default = NA
    ),
    pred = case_when(
      str_detect(variable, "VG") ~ "Caregiver",
      str_detect(variable, "VR") ~ "Recipient",
      str_detect(variable, "_P_") ~ "PTSD",
      .default = NA
    )
  ) |>
  # the PD stat needs to be calculated from the draws in their original wide format
  left_join(
    draws_estimands %>%
      select(!c(.chain, .iteration, .draw)) |>
      p_direction() |>
      tibble(),
    by = c("variable" = "Parameter")
  ) |>
  select(
    pred,
    outcome,
    effect,
    median,
    sd,
    q5,
    q95,
    pd,
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 2))) |>
  write_csv(here("output/results-posterior.csv"))


draws_estimands %>%
  select(!c(.chain, .iteration, .draw)) |>
  bayestestR::bayesfactor() |>
  tibble()
