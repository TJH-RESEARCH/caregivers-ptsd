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
  theme(
    panel.background = element_rect(fill = 'white'),
    panel.grid.major.x = element_line(color = "#8e8e8e", linewidth = .2),
    panel.grid.minor.x = element_line(color = "#8e8e8e", linewidth = .05),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_markdown(
      size = 10,
      color = "#3e3e3e",
      family = "tinos"
    ),
    axis.text.y = element_markdown(
      size = 10,
      color = "#3e3e3e",
      family = "tinos"
    ),

    axis.text.x = element_markdown(
      size = 6,
      color = "#5e5e5e",
      family = "tinos"
    ),

    # Strip customization
    strip.placement = "top",
    strip.background = element_rect(fill = "white"),
    strip.text = element_markdown(
      face = "bold",
      family = "tinos",
      color = "#3e3e3e"
    ),

    # Title customization
    plot.title = element_markdown(
      size = 20,
      color = "#3e3e3e",
      family = "tinos",
      hjust = .475
    ),
    plot.subtitle = element_markdown(size = 12, color = "#6e6e6e"),
    plot.caption = element_markdown(hjust = 0, color = "#7e7e7e"),

    # Remove the legend
    legend.position = "none"
  )

ggsave(
  here("output/plot-coefs-ptsd.pdf"),
  width = 6,
  height = 4,
  dpi = 300,
  bg = NULL
)
