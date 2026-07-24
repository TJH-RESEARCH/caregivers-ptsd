library(ggtext) # Fonts for graphs
library(showtext) # To display fonts

font_add_google("Tinos", family = "tinos")
showtext_auto()

colors_gtech <- c(
  "#8F713D",
  "#051E39",
  "#EAAA00",
  "#F9F6E5",
  "#048A81",
  "#066034",
  "#BBE6F2",
  "#D90368",
  "#660064"
)

theme_boxplot <-
  theme(
    panel.grid.major.x = element_line(color = "#e9e9e9", linewidth = .3),
    panel.grid.minor.x = element_line(color = "#e9e9e9", linewidth = .1),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = 'white', color = "#3e3e3e"),
    axis.title = element_markdown(
      size = 10,
      color = "#3e3e3e",
      family = "tinos"
    ),

    # Strip customization
    strip.background = element_rect(fill = "white"),
    strip.text = element_markdown(
      face = "bold",
      family = "tinos",
      color = "#3e3e3e"
    ),

    # Title customization
    plot.title = element_markdown(
      size = 20,
      face = "bold",
      color = "#3e3e3e",
      family = "tinos",
      hjust = .475
    ),
    plot.subtitle = element_markdown(size = 12, color = "#6e6e6e"),
    plot.caption = element_markdown(hjust = 0, color = "#7e7e7e"),

    # Remove the legend
    legend.position = "none"
  )
