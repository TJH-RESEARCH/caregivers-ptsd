draws_estimands_long |> select(outcome, effect, pred) |> distinct()

draws_estimands_long |>
  filter(pred == "PTSD") |>
  ggplot(aes(value, outcome, fill = name)) +
  ggdist::stat_halfeye(alpha = .7) +
  geom_vline(aes(xintercept = 0), linetype = 3) +
  scale_fill_manual(values = c("#8F713D", "#051E39", "#EAAA00")) +
  labs(
    title = "Effect of PTSD on Mental Health",
    x = "Standardized Coefficients",
    y = NULL
  ) +
  scale_x_continuous(limits = c(-.1, 1.25), breaks = seq(0, 1.25, .25)) +
  theme_coefs

ggsave(
  here("output/plot-coefs-ptsd-dass.pdf"),
  width = 6,
  height = 4,
  dpi = 300,
  bg = NULL
)
