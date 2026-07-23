demographics_caregiver <-
  bind_rows(
    # Gender ----------------------------------------------------------------------
    data %>%
      group_by(gender) %>%
      count_perc(sort = TRUE) %>%
      mutate(
        variable = "Gender",
        category = case_when(
          category == "male" ~ "Male",
          category == "female" ~ "Female",
          .default = category
        )
      ),

    # Race ----------------------------------------------------------------------
    data %>%
      group_by(race_asian) %>%
      count_perc(sort = FALSE) %>%
      filter(category == 1) %>%
      mutate(category = "Asian or Asia America") %>%
      mutate(variable = "Race/Ethnicity"),

    data %>%
      group_by(race_native) %>%
      count_perc(sort = FALSE) %>%
      filter(category == 1) %>%
      mutate(category = "American Indian or Alaska Native") %>%
      mutate(variable = "Race/Ethnicity"),

    data %>%
      group_by(race_black) %>%
      count_perc(sort = FALSE) %>%
      filter(category == 1) %>%
      mutate(category = "Black or African American") %>%
      mutate(variable = "Race/Ethnicity"),

    data %>%
      group_by(race_latinx) %>%
      count_perc(sort = FALSE) %>%
      filter(category == 1) %>%
      mutate(category = "Hispanic, Latino, or Spanish origin") %>%
      mutate(variable = "Race/Ethnicity"),

    data %>%
      group_by(race_mena) %>%
      count_perc(sort = FALSE) %>%
      filter(category == 1) %>%
      mutate(category = "Middle Eastern or North African") %>%
      mutate(variable = "Race/Ethnicity"),

    data %>%
      group_by(race_pacific) %>%
      count_perc(sort = FALSE) %>%
      filter(category == 1) %>%
      mutate(category = "Native Hawaiian or Other Pacific Islander") %>%
      mutate(variable = "Race/Ethnicity"),

    data %>%
      group_by(race_white) %>%
      count_perc(sort = FALSE) %>%
      filter(category == 1) %>%
      mutate(category = "White") %>%
      mutate(variable = "Race/Ethnicity"),

    data %>%
      group_by(race_other) %>%
      count_perc(sort = FALSE) %>%
      filter(category == 1) %>%
      mutate(category = "Some other race, ethnicity, or origin") %>%
      mutate(variable = "Race/Ethnicity"),

    # Education ----------------------------------------------------------------
    data %>%
      count(education) %>%
      mutate(perc = n / sum(n) * 100) %>%
      rename(category = 1) %>%
      mutate(variable = "Education Level"),

    # Employment Status ----------------------------------------------------------------
    data %>%
      count(employment) %>%
      mutate(perc = n / sum(n) * 100) %>%
      rename(category = 1) %>%
      mutate(variable = "Employment Status"),

    # Family Income -----------------------------------------------------------
    data %>%
      group_by(income) %>%
      count_perc(sort = FALSE) %>%
      mutate(variable = "Family Income (Annual)"),

    # Marital Status ----------------------------------------------------------
    data %>%
      group_by(marital) %>%
      count_perc(sort = FALSE) %>%
      mutate(variable = "Marital Status"),

    # Military Status ---------------------------------------------------------
    data %>%
      group_by(caregiver_veteran) %>%
      count_perc(sort = FALSE) %>%
      mutate(variable = "Military Status") %>%
      filter(category == 1) %>%
      mutate(category = "Current or former Military Service Member"),
  ) %>%

  # Age ---------------------------------------------------------------------
  add_case(
    category = "Mean",
    variable = "Age",
    n = data %>% summarize(mean_age = mean(age)) %>% pull(mean_age)
  ) %>%
  add_case(
    category = "SD",
    variable = "Age",
    n = data %>% summarize(sd_age = sd(age)) %>% pull(sd_age)
  ) %>%

  # Reorder columns
  select(variable, everything())


# Print to Console
demographics_caregiver %>% print(n = 100)

# Write to file
demographics_caregiver %>%
  mutate(perc = round(perc / 100, 3)) |>
  write_csv(here::here("output/demographics-caregiver.csv"))
