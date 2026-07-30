# Select a color palette to represent the uni
colors_gtech_coefs <- c(
  "#8F713D",
  "#B39051",
  "#DEBD88",
  "#051E39",
  "#003D69",
  "#004C97"
)

draws_estimands_long <-
  draws_estimands %>%
  dplyr::select(
    starts_with("CDE_VG"),
    starts_with("TE_VR"),
    starts_with("TE_P"),
    starts_with("DE_VG_P"),
    starts_with("TE_VR_P")
  ) %>%
  pivot_longer(everything()) %>%
  mutate(
    outcome = case_when(
      str_detect(name, "_D") ~ "Depression",
      str_detect(name, "_A") ~ "Anxiety",
      str_detect(name, "_S") ~ "Stress",
      str_detect(name, "_P") ~ "PTSD",
      .default = NA
    ),
    effect = case_when(
      str_detect(name, "CDE") ~ "Conditional Direct",
      str_detect(name, "DE") ~ "Direct",
      str_detect(name, "TE") ~ "Total",
      .default = NA
    ),
    pred = case_when(
      str_detect(name, "VG") ~ "Caregiver",
      str_detect(name, "VR") ~ "Recipient",
      str_detect(name, "_P_") ~ "PTSD",
      .default = NA
    )
  )

draws_estimands_long |> select(outcome, effect, pred) |> distinct()

draws_estimands_long |>
  filter(pred != "PTSD" & outcome != "PTSD") |>
  ggplot(aes(value, outcome, fill = name)) +
  ggdist::stat_halfeye(alpha = .7) +
  geom_vline(aes(xintercept = 0), linetype = 3) +
  facet_wrap(vars(pred), ncol = 1) +
  scale_fill_manual(values = colors_gtech_coefs) +
  labs(
    title = "Effect of Veteran Status on Mental Health",
    x = "Standardized Coefficients",
    y = NULL
  ) +
  scale_x_continuous(limits = c(-.5, .75), breaks = seq(-.5, .75, .25)) +
  theme_coefs

ggsave(
  here("output/plot-coefs-das.pdf"),
  width = 6,
  height = 4,
  dpi = 300,
  bg = NULL
)
