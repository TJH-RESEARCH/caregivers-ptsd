
# Contrast in means
## Given these are binary variables, the coefficients are equal to the contrast in means. 


# Fix names
names(draws_estimands) <- str_replace_all(names(draws_estimands), replacements)

draws_estimands %>% posterior::summarise_draws()
