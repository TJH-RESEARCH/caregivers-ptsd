draws_estimands_long |> select(outcome, effect, pred) |> distinct()

draws_estimands_long |>
  filter(outcome == "PTSD") |>
  ggplot(aes(value, pred, fill = name)) +
  ggdist::stat_halfeye(alpha = .7) +
  geom_vline(aes(xintercept = 0), linetype = 3) +
  scale_fill_manual(values = c("#8F713D", "#051E39")) +
  labs(
    title = "Effect of Veteran Status on PTSD",
    x = "Standardized Coefficients",
    y = NULL
  ) +
  scale_x_continuous(limits = c(-.5, 2), breaks = seq(-.5, 2, .5)) +
  theme_coefs

ggsave(
  here("output/plot-coefs-ptsd.pdf"),
  width = 6,
  height = 4,
  dpi = 300,
  bg = NULL
)
