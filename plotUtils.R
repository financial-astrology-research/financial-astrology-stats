# Title     : R plot / chart utilities.
# Objective : Help with data visualizations.
# Created by: pablocc
# Created on: 27/02/2021

library(ggplot2)

ggplotDarkTheme <- function(base_size = 12) {
  theme_grey(base_size = base_size) %+replace%
    theme(
      axis.line = element_blank(),
      axis.text.x = element_text(size = base_size * 0.8, color = "white", lineheight = 0.9),
      axis.text.y = element_text(size = base_size * 0.8, color = "white", lineheight = 0.9),
      axis.ticks = element_line(color = "white", size = 0.2),
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(10, 0, 10, 0)),
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 10)),
      axis.ticks.length = unit(0.3, "lines"),
      legend.background = element_rect(color = NA, fill = "black"),
      legend.key = element_rect(color = "white", fill = "black"),
      legend.key.size = unit(1.2, "lines"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.text = element_text(size = base_size * 1.1, color = "white"),
      legend.title = element_text(size = base_size * 1.3, face = "bold", hjust = 0, color = "white"),
      legend.position = "bottom",
      legend.box = NULL,
      legend.text.align = NULL,
      legend.title.align = NULL,
      legend.direction = NULL,
      panel.background = element_rect(fill = "black", color = NA),
      panel.border = element_rect(fill = NA, color = "white"),
      panel.grid.major = element_line(color = "grey35"),
      panel.grid.minor = element_line(color = "grey20"),
      panel.spacing = unit(0.5, "lines"),
      strip.background = element_rect(fill = "grey30", color = "grey10"),
      strip.text.x = element_text(size = base_size * 0.8, color = "white"),
      strip.text.y = element_text(size = base_size * 0.8, color = "white", angle = -90),
      plot.background = element_rect(color = "black", fill = "black"),
      plot.title = element_text(
        size = base_size * 1.2,
        color = "white",
        margin = margin(10, 0, 10, 0)
      ),
      plot.margin = unit(rep(1, 4), "lines")
    )
}