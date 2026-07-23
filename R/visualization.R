library(ggsci)

data |>
  select(
    caregiver_veteran,
    recipient_veteran,
    dass_depression,
    dass_anxiety,
    dass_stress
  ) |>
  pivot_longer(c(dass_depression, dass_anxiety, dass_stress)) |>
  mutate(caregiver_veteran = as.factor(caregiver_veteran)) |>
  ggplot(aes(caregiver_veteran, value)) +
  geom_boxplot() +
  facet_grid(~name)


data |>
  select(
    caregiver_veteran,
    recipient_veteran,
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
    recipient_veteran = factor(
      recipient_veteran,
      levels = c(0, 1),
      labels = c("Civilian", "Veteran")
    )
  ) |>
  ggplot(aes(recipient_veteran, value)) +
  geom_boxplot() +
  geom_jitter(
    height = .6,
    width = .2,
    aes(color = recipient_veteran),
    alpha = .5
  ) +
  labs(
    title = "Mental Health Symptoms by Recipient Veteran Status",
    x = "Recipient",
    y = "Symptom Severity (DASS score)"
  ) +
  facet_grid(~name) +
  ggsci::scale_color_bmj() +
  theme(
    text = element_text(size = 24),
    panel.background = element_rect(fill = 'white'),
    panel.grid.major.y = element_line(color = "#e3e3e3"),
    panel.grid.minor.y = element_line(color = "#e3e3e3"),
    axis.ticks = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = 'bold'),
    legend.position = "none"
  )

ggsave(
  here("output/boxplot-recipient.jpg"),
  width = 6,
  height = 4,
  dpi = 300,
  bg = NULL
)
