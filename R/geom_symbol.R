#' Points with a Wider Variety of Shapes
#'
#' The symbol geom is used for the same purposes as [geom_point()], but with
#' more flexibility in the shapes of the points.
#'
#' Custom symbol shapes can be defined using `symbol_recipe()`.
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
                                 symbol = 1, colour = "black", size = 1.5, fill = NA,
                                 alpha = NA, stroke = 1.42),

                               # the function to draw symbols placed according to 'x', 'y', and 'symbol' aesthetics
                               draw_panel = function(self, data, panel_params, coord, na.rm = FALSE) {
                                 coords <- coord$transform(data, panel_params)

                                 # a list of grobs is created with one symbol for each data point
                                 grob_list <- lapply(seq(nrow(coords)), function(i_symbol) {

                                   symbol <- coords$symbol[i_symbol]

                                   # extract symbol details according to data type
                                   if (class(symbol) != "character") {
                                     symbol_details <- symbol_lookup[[symbol]]
                                   } else if (grepl("symbol_", symbol)) {
                                     symbol_details <- symbol_recipe_to_list(symbol)
                                   } else if (symbol %in% names(symbol_lookup)) {
                                     symbol_details <- symbol_lookup[[symbol]]
                                   } else {
                                     stop("Symbol values not recognised, Consider supplying data to the symbol aesthetic as a factor.")
                                   }

                                   if (head(symbol_details$x, 1) == tail(symbol_details$x, 1) &
                                       head(symbol_details$y, 1) == tail(symbol_details$y, 1)) {

                                     # if the path begins and ends in the same place,
                                     # a solid polygon with an outline and fill parameter is created
                                     grob_output <- polygonGrob(x = unit(coords$x[i_symbol], "native") +
                                                                  unit(symbol_details$x * coords$size[i_symbol], "mm"),
                                                                y = unit(coords$y[i_symbol], "native") +
                                                                  unit(symbol_details$y * coords$size[i_symbol], "mm"),
                                                                gp = gpar(col = alpha(coords$colour[i_symbol], coords$alpha[i_symbol]),
                                                                          fill = alpha(coords$fill[i_symbol], coords$alpha[i_symbol]),
                                                                          lwd = coords$stroke[i_symbol]))
                                   } else {

                                     # if the outline does not begin and end in the same place
                                     # a line is created with an outline but no fill parameter
                                     grob_output <- polylineGrob(x = unit(coords$x[i_symbol], "native") +
                                                                   unit(symbol_details$x * coords$size[i_symbol], "mm"),
                                                                 y = unit(coords$y[i_symbol], "native") +
                                                                   unit(symbol_details$y * coords$size[i_symbol], "mm"),
                                                                 id = symbol_details$id,
                                                                 gp = gpar(col = alpha(coords$colour[i_symbol], coords$alpha[i_symbol]),
                                                                           lwd = coords$stroke[i_symbol]))
                                   }

                                   # output each individual data point symbol
                                   grob_output
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
