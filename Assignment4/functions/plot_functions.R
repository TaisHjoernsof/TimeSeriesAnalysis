# ---- Set a global ggplot2 plotting theme ----
set_plot_style <- function(base_size = 16) {
  library(ggplot2)
  theme_set(
    theme_minimal(base_size = base_size) +
      theme(
        plot.title = element_text(face = "bold", size = base_size + 2),
        axis.title = element_text(size = base_size),
        axis.text = element_text(size = base_size - 2),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank()
      )
  )
}
