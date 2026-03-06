
# Contrast in means
## Given these are binary variables, the coefficients are equal to the contrast in means. 

draws_estimands %>% posterior::summarise_draws()
draws_variance %>% posterior::summarise_draws()