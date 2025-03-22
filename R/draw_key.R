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
            size = data$size %||% 3,
            fill = data$fill %||% "black",
            linewidth2 = data$linewidth2 %||% 1.42,
            fill2 = data$fill2 %||% "white",
            colour = data$colour %||% "black",
            linewidth = data$linewidth %||% 1.42)
}

#' @export
#' @rdname draw_key_plottwist2
draw_key_symbol <- function(data, params, size) {

  symbol <- data$symbol

  # extract symbol details according to data type
  if (class(symbol) != "character") {
    symbol_details <- symbol_lookup[[symbol]]
  } else if (grepl("symbol_", symbol)) {
    symbol_details <- symbol_recipe_to_list(symbol)
  } else if (symbol %in% names(symbol_lookup)) {
    symbol_details <- symbol_lookup[[symbol]]
  } else {
    stop("symbol values not recognised")
  }

  if (head(symbol_details$x, 1) == tail(symbol_details$x, 1) &
      head(symbol_details$y, 1) == tail(symbol_details$y, 1)) {
    geom_output <- polygonGrob(x = unit(0.5, "npc") +
                                 unit(symbol_details$x * data$size, "mm"),
                               y = unit(0.5, "npc") +
                                 unit(symbol_details$y * data$size, "mm"),
                               gp = gpar(col = alpha(data$colour %||% "black",
                                                     data$alpha),
                                         fill = alpha(data$fill %||% "black",
                                                      data$alpha),
                                         lwd = data$stroke %||% 0.5))
  } else {
    geom_output <- polylineGrob(x = unit(0.5, "npc") +
                                  unit(symbol_details$x * data$size, "mm"),
                                y = unit(0.5, "npc") +
                                  unit(symbol_details$y * data$size, "mm"),
                                id = symbol_details$id,
                                gp = gpar(col = alpha(data$colour %||% "black",
                                                      data$alpha),
                                          lwd = data$stroke %||% 0.5))
  }
  geom_output
}
