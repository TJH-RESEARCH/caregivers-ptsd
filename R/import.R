# IMPORT DATA

## Read the data from file and save to environment as 'data'
data <- read_csv(here::here("data/data-caregivers-cleaned-2026-01-29.csv"))

## Rename the variables to something clearer
data <-
  data %>% 
  rename(recipient_veteran = care_military,
         caregiver_veteran = military
  )