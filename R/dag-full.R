# Complete DAG to analyze for covariate sets

# Specify the DAG
dag_full <- 
  dagify(
    DAS ~ VG + VR + P + H + C + I + E + G + A + R,
    VR ~ `F` + M,
    VG ~ `F` + S + G + A + R,
    P ~ VR + VG + G + A + R + I + E, 
    H ~ VR + C,
    C ~ VR,
    K ~ VR + VG,
    M ~ VG,
    S ~ `F` + R,
    E ~ VG + S + G + A + R,
    I ~ VG + S + E + G + A + R)

# Plot the DAG
plot_dag_full <- dag_full %>%  ggdag() + ggdag::theme_dag()

# Save DAG plot to file
ggsave(plot = plot_dag_full, here::here('output/dag-full.jpg'), height = 4, width = 6)

