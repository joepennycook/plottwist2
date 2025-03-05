theme_simple <- function() {
  ggplot2::theme(axis.title = ggtext::element_markdown(),
        axis.text = ggtext::element_markdown(),
        strip.text = ggtext::element_markdown(),
        legend.title = ggtext::element_markdown(),
        legend.text = ggtext::element_markdown(),
        axis.line = ggplot2::element_blank(),
        strip.background = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        legend.background = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "white",
                                                 colour = "black",
                                                 linewidth = 1),
        legend.title.align = 0.5)
}
