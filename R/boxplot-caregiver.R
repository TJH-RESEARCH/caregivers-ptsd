data |>
  select(
    caregiver_veteran,
    dass_depression,
    dass_anxiety,
    dass_stress
  ) |>
  pivot_longer(c(dass_depression, dass_anxiety, dass_stress)) |>
  mutate(
    name = case_when(
      name == "dass_depression" ~ "Depression",
      name == "dass_anxiety" ~ "Anxiety",
      name == "dass_stress" ~ "Stress",
      .default = NA
    ),
    caregiver_veteran = factor(
      caregiver_veteran,
      levels = c(0, 1),
      labels = c("Civilian", "Veteran")
    )
  ) |>
  ggplot(aes(value, caregiver_veteran)) +
  geom_boxplot(staplewidth = .5) +
  geom_jitter(
    height = .2,
    width = .6,
    aes(color = caregiver_veteran),
    alpha = .5
  ) +
  labs(
    title = "Symptoms by Caregiver Veteran Status",
    x = "Symptom Severity (DASS score)",
    y = NULL
  ) +
  facet_grid(~name) +
  scale_color_manual(values = colors_gtech) +
  lims(x = c(-1, 43)) +
  theme_boxplot

ggsave(
  here("output/boxplot-caregiver.pdf"),
  width = 6,
  height = 4,
  dpi = 300,
  bg = NULL
)
