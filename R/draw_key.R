#' More key glyphs for legends
#'
#' An expansion of the draw_key_x functions from ggplot2 to include the new
#' geoms included in plottwist2.
#'
#' @inheritParams ggplot2::draw_key_point
#' @name draw_key_plottwist2

#' @export
#' @rdname draw_key_plottwist2
draw_key_patch <- function(data, params, size) {

  patchGrob(xmin = unit(0.1, "npc"),
            xmax = unit(0.9, "npc"),
            ymin = unit(0.1, "npc"),
            ymax = unit(0.9, "npc"),
            pattern = data$pattern %||% 1,
            spacing = unit(data$spacing %||% 1, "mm"),
            p_colour = data$p_colour %||% "black",
            p_linewidth = data$p_linewidth %||% 1,
            fill = data$fill %||% "white",
            gp = gpar(col = data$colour %||% "black",
                      lwd = data$linewidth %||% 1.42,
                      lty = data$linetype %||% 1))
}

#' @export
#' @rdname draw_key_plottwist2
draw_key_symbol <- function(data, params, size) {
  symbol_recipe <- symbol_recipes[[data$symbol]]
  if (head(symbol_recipe$x, 1) == tail(symbol_recipe$x, 1) &
      head(symbol_recipe$y, 1) == tail(symbol_recipe$y, 1)) {
    geom_output <- polygonGrob(x = unit(0.5, "npc") + unit(symbol_recipe$x * data$size, "mm"),
                               y = unit(0.5, "npc") + unit(symbol_recipe$y * data$size, "mm"),
                               gp = gpar(col = alpha(data$colour %||% "black", data$alpha),
                                         fill = alpha(data$fill %||% "black", data$alpha),
                                         lwd = data$stroke %||% 0.5))
  } else {
    geom_output <- polylineGrob(x = unit(0.5, "npc") + unit(symbol_recipe$x * data$size, "mm"),
                                y = unit(0.5, "npc") + unit(symbol_recipe$y * data$size, "mm"),
                                id = symbol_recipe$poly,
                                gp = gpar(col = alpha(data$colour %||% "black", data$alpha),
                                          lwd = data$stroke %||% 0.5))
  }
  geom_output
}
