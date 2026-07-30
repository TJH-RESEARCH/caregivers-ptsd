# Simple DAG of poster variables to print

# specify the DAG
dag_poster <-
  dagify(
    DAS ~ P + VG + VR,
    P ~ VR + VG,
    VR ~ `F` + M,
    VG ~ `F`,
    M ~ VG,
    exposure = 'P',
    coords = list(
      x = c(VR = -.15, VG = -.15, P = .05, DAS = .3, `F` = -.40, M = -.15),
      y = c(VR = -.07, VG = .07, P = 0, DAS = 0, `F` = 0, M = 0)
    )
  )

dag_poster <- setVariableStatus(dag_poster, "latent", c("M", "F"))
adjustmentSets(dag_poster, exposure = "VR", outcome = "DAS", effect = "total")
adjustmentSets(dag_poster, exposure = "VR", outcome = "DAS", effect = "direct")
adjustmentSets(dag_poster, exposure = "VG", outcome = "DAS", effect = "total")

# Add descriptive labels
dag_poster_tidy <-
  dag_poster %>%
  tidy_dagitty() %>%
  mutate(
    label = case_when(
      name == "VG" ~ "Vet Giver",
      name == "VR" ~ "Vet Recipient",
      name == "P" ~ "PTSD",
      name == "DAS" ~ "DAS",
      name == "F" ~ "Family",
      name == "M" ~ "Marriage",
      .default = name
    )
  )

# Create a stylized graph
plot_dag_poster <-
  dag_poster_tidy %>%
  ggplot(aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend)) +
  geom_dag_text(
    color = 'black',
    size = 34,
    family = "karla",
    fontface = "plain"
  ) +
  geom_dag_text(
    aes(label = label),
    size = 12,
    color = colors_johnson[4],
    nudge_y = -.0185,
    nudge_x = -.000005,
    fontface = "italic",
    family = "cardo"
  ) +
  geom_dag_edges(
    curvature = .5,
    start_cap = ggraph::circle(15, 'mm'),
    end_cap = ggraph::circle(12, 'mm'),
    edge_color = "grey10",
    edge_width = .45
  ) +
  theme_dag_blank() +
  lims(x = c(-.50, .40))

# Save stylized graph to file
ggsave(
  plot = plot_dag_poster,
  here::here('output/dag-poster.jpg'),
  height = 4,
  width = 6
)
