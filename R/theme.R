font_add_google("Karla", family = "karla")
font_add_google("Cardo", family = "cardo")
showtext_auto()
colors_johnson <- MetBrewer::MetPalettes$Johnson[[1]]
c("#a00e00", "#d04e00", "#f6c200", "#0086a8", "#132b69")


# Create a custom theme for density plots
theme_density <-
  theme(
    # Remove the legend
    legend.position = 'none',
    
    # Panel customization
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid.major.x = element_line(color = "#e9e9e9", linewidth = .3),
    panel.grid.minor.x = element_line(color = "#e9e9e9", linewidth = .1),
    panel.grid.major.y = element_blank(),
    
    # Axis customization
    #axis.line = element_line(color = "#3e3e3e"),
    axis.line.y.left = element_blank(),
    axis.title = element_markdown(size = 10, face = "bold"),
    axis.title.y = element_markdown(margin = margin(r = 20)),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    
    # Title customization
    plot.title = element_markdown(size = 14, face = "bold"),
    plot.subtitle = element_markdown(size = 12, color = "#3e3e3e"),
    plot.caption = element_markdown(hjust = 0, color = "#7e7e7e"),
    
    # Strip customization
    strip.background = element_rect(fill = "white"), 
    strip.text = element_text(face = "bold")
  )