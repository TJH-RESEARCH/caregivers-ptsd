# Simple DAG of core variables to print

# specify the DAG
dag_core <- 
  dagify(
    DAS ~ P + VG + VR,
    P ~ VR + VG,
    exposure = 'P', 
    coords = list(
      x = c(VR = -.15, VG = -.15, P = .05, DAS = .3),
      y = c(VR = -.07,VG = .07, P = 0, DAS = 0))
  )

# Add descriptive labels
dag_core_tidy <-  
  dag_core %>% 
  tidy_dagitty() %>% 
  mutate(
    label = case_when(
      name == "VG" ~ "Vet Giver",
      name == "VR" ~ "Vet Recipient",
      name == "P" ~ "PTSD", 
      name == "DAS" ~ "DAS", 
      .default = name))

# Create a stylized graph
plot_dag_core <-
  dag_core_tidy %>% 
  ggplot(aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend)) +
  geom_dag_text(color = 'black', size = 34, family = "karla", fontface = "plain") +
  geom_dag_text(aes(label = label), size = 12, color = colors_johnson[4], nudge_y = -.0185, nudge_x = -.000005, fontface = "italic", family = "cardo") +
  geom_dag_edges(
    curvature = .5, 
    start_cap = ggraph::circle(15, 'mm'),
    end_cap = ggraph::circle(12, 'mm'), 
    edge_color = "grey10", 
    edge_width = .45
  ) + 
  theme_dag_blank() +
  lims(x = c(-.45,.40))

# Save stylized graph to file
ggsave(plot = plot_dag_core, here::here('output/dag-core.jpg'), height = 4, width = 6)