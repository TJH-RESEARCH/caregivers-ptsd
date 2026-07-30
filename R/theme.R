# Load fonts
font_add_google("Karla", family = "karla")
font_add_google("Cardo", family = "cardo")
font_add_google("Tinos", family = "tinos") # this is like times new roman
showtext_auto()

# Select a color palette to represent the uni
colors_gtech <- c(
  "#8F713D",
  "#051E39",
  "#EAAA00",
  "#048A81",
  "#066034",
  "#D90368",
  "#660064",
  "#BBE6F2",
  "#F9F6E5"
)

# another color palette for fun
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
    axis.title = element_markdown(size = 14, face = "bold"),
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

# Create a theme for boxplots
theme_boxplot <-
  theme(
    panel.background = element_rect(fill = 'white', color = "#3e3e3e"),
    panel.grid.major.x = element_line(color = "#e9e9e9", linewidth = .3),
    panel.grid.minor.x = element_line(color = "#e9e9e9", linewidth = .1),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_markdown(
      size = 12,
      color = "#3e3e3e",
      family = "tinos"
    ),
    axis.text.y = element_markdown(
      size = 16,
      color = "#3e3e3e",
      family = "tinos"
    ),

    axis.text.x = element_markdown(
      size = 12,
      color = "#5e5e5e",
      family = "tinos"
    ),

    # Strip customization
    strip.background = element_rect(fill = "white"),
    strip.text = element_markdown(
      size = 12,
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


theme_coefs <-
  theme(
    panel.background = element_rect(fill = 'white'),
    panel.grid.major.x = element_line(color = "#8e8e8e", linewidth = .2),
    panel.grid.minor.x = element_line(color = "#8e8e8e", linewidth = .05),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_markdown(
      size = 14,
      color = "#3e3e3e",
      family = "tinos"
    ),
    axis.text.y = element_markdown(
      size = 14,
      color = "#3e3e3e",
      family = "tinos"
    ),

    axis.text.x = element_markdown(
      size = 10,
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
