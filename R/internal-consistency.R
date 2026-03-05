
# Depression DASS-21 -----------------------------------------------------------
fit_depression <- 
  lavaan::cfa(model = "depression =~ dass_3 + dass_5 + dass_10 + dass_13 + 
                                     dass_16 + dass_17 + dass_21", 
              data,
              std.lv = TRUE,
              ordered = c('dass_3','dass_5','dass_10', 'dass_13','dass_16',
                          'dass_17', 'dass_21'),
              estimator = "WLSMV")

reliability_depression <- 
  semTools::compRelSEM(fit_depression,
                       obs.var = TRUE,
                       tau.eq = FALSE,
                       ord.scale = TRUE,
                       return.df = TRUE)



# Anxiety DASS-21 --------------------------------------------------------------
fit_anxiety <- 
  lavaan::cfa(model = "anxiety =~ dass_2 + dass_4 + dass_7 + dass_9 + dass_15 + 
                                  dass_19 + dass_20", 
              data,
              std.lv = TRUE,
              ordered = c('dass_2','dass_4', 'dass_7', 'dass_9', 'dass_15',
                          'dass_19', 'dass_20'),
              estimator = "WLSMV")

reliability_anxiety <- 
  semTools::compRelSEM(fit_anxiety,
                       obs.var = TRUE,
                       tau.eq = FALSE,
                       ord.scale = TRUE,
                       return.df = TRUE)


# Stress DASS-21 ---------------------------------------------------------------
fit_stress <- 
  lavaan::cfa(model = "stress =~ dass_1 + dass_6 + dass_8 + dass_11 + dass_12 + 
                                 dass_14 + dass_18", 
              data,
              std.lv = TRUE,
              ordered = c('dass_1','dass_6','dass_8', 'dass_11', 'dass_12',
                          'dass_14', 'dass_18'),
              estimator = "WLSMV")

reliability_stress <- 
  semTools::compRelSEM(fit_stress,
                       obs.var = TRUE,
                       tau.eq = FALSE,
                       ord.scale = TRUE,
                       return.df = TRUE)


# Make a table of results -------------------------------------------------
internal_consistency <-
  tibble(
    depression = as.numeric(reliability_depression),
    anxiety =  as.numeric(reliability_anxiety),
    stress =  as.numeric(reliability_stress),
  )

# Print to console
internal_consistency %>% print()

# Write to file
internal_consistency %>% write_csv(here("output/internal-consistency.csv"))
