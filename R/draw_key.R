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
            pattern = data$pattern %||% 11,
            pattern_scale = data$pattern_scale %||% 1.42,
            fill = data$fill %||% "black",
            pattern_linewidth = data$pattern_linewidth %||% 1.42,
            pattern_background = data$pattern_background %||% "white",
            colour = data$colour %||% "black",
            linewidth = data$linewidth %||% 1)
}

#' @export
#' @rdname draw_key_plottwist2
draw_key_symbol <- function(data, params, size) {

  symbol <- data$symbol

  # extract symbol details according to data type
  if (!is.character(symbol)) {
    symbol_details <- symbol_lookup[[symbol]]
  } else if (grepl("symbol_", symbol)) {
    symbol_details <- symbol_recipe_to_list(symbol)
  } else if (symbol %in% names(symbol_lookup)) {
    symbol_details <- symbol_lookup[[symbol]]
  } else {
    stop("Symbol values not recognised, Consider supplying data to the symbol aesthetic as a factor.")
  }

  if (symbol_details$autofill == TRUE) {
    fill_value <- alpha(coords$colour[i_symbol], coords$alpha[i_symbol])
  } else {
    fill_value <- alpha(coords$fill[i_symbol], coords$alpha[i_symbol])
  }

  grobs_output <- list()

  # if there are coordinates to draw the filled shape
  # add that to the output
  if (!is.na(symbol_details$solid_x[1])) {
    grob_fill <- polygonGrob(x = unit(coords$x[i_symbol], "native") +
                               unit(symbol_details$solid_x * coords$size[i_symbol], "mm"),
                             y = unit(coords$y[i_symbol], "native") +
                               unit(symbol_details$solid_y * coords$size[i_symbol], "mm"),
                             gp = gpar(col = "#FFFFFF00",
                                       fill = fill_value,
                                       lwd = coords$stroke[i_symbol]))
  } else {
    grob_fill <- polygonGrob(x = c(0), y = c(0))
  }

  # if there are coordinates to draw the outline
  # add that to the output
  if (!is.na(symbol_details$x[1])) {
    grob_line <- polylineGrob(x = unit(coords$x[i_symbol], "native") +
                                unit(symbol_details$x * coords$size[i_symbol], "mm"),
                              y = unit(coords$y[i_symbol], "native") +
                                unit(symbol_details$y * coords$size[i_symbol], "mm"),
                              id = symbol_details$id,
                              gp = gpar(col = alpha(coords$colour[i_symbol], coords$alpha[i_symbol]),
                                        lwd = coords$stroke[i_symbol]))
  } else {
    grob_line <- polylineGrob(x = c(0), y = c(0))
  }

  # output each individual data point symbol
  grobTree(grob_fill, grob_line)
}
