demographics_recipient <-
  bind_rows(
    # Gender ----------------------------------------------------------------------
    data %>%
      group_by(caregivee_gender) %>%
      count_perc(sort = TRUE) %>%
      mutate(variable = "Gender"),

    # Relationship ------------------------------------------------------------
    data %>%
      group_by(caregivee_relation) %>%
      count_perc(sort = TRUE) %>%
      mutate(variable = "Relation to Caregiver"),

    # Military Status ---------------------------------------------------------
    data %>%
      group_by(recipient_veteran) %>%
      count_perc(sort = FALSE) %>%
      mutate(variable = "Military Status") %>%
      filter(category == 1) %>%
      mutate(category = "Current or former Military Service Member"),

    # Recipient Condition ---------------------------------------------------------
    data %>%
      group_by(caregivee_condition) %>%
      count_perc(sort = FALSE) %>%
      mutate(variable = "Condition"),

    # Recipient Location ---------------------------------------------------------
    data %>%
      group_by(caregivee_location) %>%
      count_perc(sort = FALSE) %>%
      mutate(variable = "Care Location"),

    # Care Tenure ---------------------------------------------------------
    data %>%
      group_by(care_tenure) %>%
      count_perc(sort = FALSE) %>%
      mutate(variable = "Tenure"),

    # Care Tenure ---------------------------------------------------------
    data %>%
      group_by(hours) %>%
      count_perc(sort = FALSE) %>%
      mutate(variable = "Hours per Week"),
  ) %>%

  # Age ---------------------------------------------------------------------
  add_case(
    category = "Mean",
    variable = "Age",
    n = data %>% summarize(mean_age = mean(caregivee_age)) %>% pull(mean_age)
  ) %>%
  add_case(
    category = "SD",
    variable = "Age",
    n = data %>% summarize(sd_age = sd(caregivee_age)) %>% pull(sd_age)
  ) %>%

  # Reorder columns
  select(variable, everything())


# Print to Console
demographics_recipient %>% print(n = 100)

# Write to file
demographics_recipient %>%
  write_csv(here::here("output/demographics-recipient.csv"))
