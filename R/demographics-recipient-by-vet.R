demographics_recipient_by_vet <-
  bind_rows(
    # Gender ----------------------------------------------------------------------
    data %>%
      group_by(caregivee_gender, recipient_veteran) %>%
      count(caregivee_gender) %>%
      group_by(recipient_veteran) %>%
      mutate(perc = n / sum(n)) %>%
      mutate(variable = "Gender") %>%
      rename(category = 1) %>%
      mutate(category = as.character(category)),

    # Relationship ------------------------------------------------------------
    data %>%
      group_by(caregivee_relation, recipient_veteran) %>%
      count(caregivee_relation) %>%
      group_by(recipient_veteran) %>%
      mutate(perc = n / sum(n)) %>%
      mutate(variable = "Relation to Caregiver") %>%
      rename(category = 1) %>%
      mutate(category = as.character(category)),

    # Recipient Condition ---------------------------------------------------------
    data %>%
      group_by(caregivee_condition, recipient_veteran) %>%
      count(caregivee_condition) %>%
      group_by(recipient_veteran) %>%
      mutate(perc = n / sum(n)) %>%
      mutate(variable = "Condition") %>%
      rename(category = 1) %>%
      mutate(category = as.character(category)),

    # Recipient Location ---------------------------------------------------------
    data %>%
      group_by(caregivee_location, recipient_veteran) %>%
      count(caregivee_location) %>%
      group_by(recipient_veteran) %>%
      mutate(perc = n / sum(n)) %>%
      mutate(variable = "Care Location") %>%
      rename(category = 1) %>%
      mutate(category = as.character(category)),

    # Care Tenure ---------------------------------------------------------
    data %>%
      group_by(care_tenure, recipient_veteran) %>%
      count(care_tenure) %>%
      group_by(recipient_veteran) %>%
      mutate(perc = n / sum(n)) %>%
      mutate(variable = "Tenure") %>%
      rename(category = 1) %>%
      mutate(category = as.character(category)),

    # Care Tenure ---------------------------------------------------------
    data %>%
      group_by(hours, recipient_veteran) %>%
      count(hours) %>%
      group_by(recipient_veteran) %>%
      mutate(perc = n / sum(n)) %>%
      mutate(variable = "Hours per Week") %>%
      rename(category = 1) %>%
      mutate(category = as.character(category)),
  ) %>%
  mutate(
    recipient_veteran = ifelse(recipient_veteran == 1, "Veteran", "Non_Veteran")
  ) %>%
  pivot_wider(
    id_cols = c(variable, category),
    values_from = c(n, perc),
    names_from = recipient_veteran
  ) %>%

  # Age ---------------------------------------------------------------------
  add_case(
    category = "Mean",
    variable = "Age",
    n_Non_Veteran = data %>%
      filter(recipient_veteran == 0) %>%
      summarize(mean_age = mean(caregivee_age)) %>%
      pull(mean_age),
    n_Veteran = data %>%
      filter(recipient_veteran == 1) %>%
      summarize(mean_age = mean(caregivee_age)) %>%
      pull(mean_age)
  ) %>%
  add_case(
    category = "SD",
    variable = "Age",
    n_Non_Veteran = data %>%
      filter(recipient_veteran == 0) %>%
      summarize(sd_age = sd(caregivee_age)) %>%
      pull(sd_age),
    n_Veteran = data %>%
      filter(recipient_veteran == 1) %>%
      summarize(sd_age = sd(caregivee_age)) %>%
      pull(sd_age)
  ) %>%

  # Reorder columns
  select(variable, everything())


# Print to Console
demographics_recipient_by_vet %>% print(n = 100)

# Write to file
demographics_recipient_by_vet %>%
  mutate(across(starts_with('perc'), ~ round(.x, 3))) %>%
  write_csv(here::here("output/demographics-recipient-by-vet.csv"))
