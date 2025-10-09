#' Points with a Wider Variety of Shapes
#'
#' The symbol geom is primarily used to create scatterplots in the same way as
#' \link[ggplot2]{ggplot2::geom_point()}, but allows for more flexibility in the
#' shape of the points.
#'
#' The 'symbol' aesthetic is used to define the shape of points. Numeric values
#' between `1` and `40` supplied to the symbol aesthetic will select from a
#' range of shapes. Character values such as `"triangle"`, `"hollow_diamond"`,
#' `"hexagon_cross"` or `"octogon_zigzag"` will also select shapes.
#'
#' Custom shapes can be defined using \link{symbol_recipe()}.
#'
#' @inheritParams ggplot2::geom_point
#'
#' @returns
#' @import ggplot2
#' @import grid
#' @import rlang
#' @export
#'
#' @examples
geom_symbol <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSymbol,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

GeomSymbol <- ggplot2::ggproto("GeomSymbol", Geom,
                               required_aes = c("x", "y"),
                               non_missing_aes = c("size", "symbol", "colour"),
                               default_aes = aes(
                                 symbol = 1, colour = "black", size = 1.5, fill = "#FFFFFF00",
                                 alpha = NA, stroke = 1.42),

                               # the function to draw symbols placed according to 'x', 'y', and 'symbol' aesthetics
                               draw_panel = function(self, data, panel_params, coord, na.rm = FALSE) {
                                 coords <- coord$transform(data, panel_params)

                                 # a list of grobs is created with one symbol for each data point
                                 grob_list <- lapply(seq(nrow(coords)), function(i_symbol) {

                                   symbol <- coords$symbol[i_symbol]

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
                                 })


                                 # collect all the symbols into a single grob with grobTree
                                 do.call(grobTree, grob_list)
                               },

                               # function to draw correct symbol in legend key
                               draw_key = draw_key_symbol
)

# below copied directly from ggplot2 package 17/03/2023
manual_scale <- function(aesthetic, values = NULL, breaks = waiver(), ..., limits = NULL) {
  # check for missing `values` parameter, in lieu of providing
  # a default to all the different scale_*_manual() functions
  if (is_missing(values)) {
    values <- NULL
  } else {
    force(values)
  }

  if (is.null(limits) && !is.null(names(values))) {
    # Limits as function to access `values` names later on (#4619)
    limits <- function(x) intersect(x, names(values)) %||% character()
  }

  # order values according to breaks
  if (is.vector(values) && is.null(names(values)) && !is.waive(breaks) &&
      !is.null(breaks) && !is.function(breaks)) {
    if (length(breaks) <= length(values)) {
      names(values) <- breaks
    } else {
      names(values) <- breaks[1:length(values)]
    }
  }

  pal <- function(n) {
    if (n > length(values)) {
      cli::cli_abort("Insufficient values in manual scale. {n} needed but only {length(values)} provided.")
    }
    values
  }
  discrete_scale(aesthetic, "manual", pal, breaks = breaks, limits = limits, ...)
}

waiver <- function() structure(list(), class = "waiver")

is.waive <- function(x) inherits(x, "waiver")
