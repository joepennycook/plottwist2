#' Create more discrete scales
#'
#' These functions allow you to specify your own set of mappings from levels in
#' the data to the new aesthetics included in plottwist2, following the format
#' of the scale_manual_x functions in ggplot2.
#'
#' @inheritParams ggplot2::scale_colour_manual
#' @name scale_manual_plottwist2
#'
#' @examples
#' ggplot2::ggplot(data = iris,
#' ggplot2::aes(x = Sepal.Length,
#'              y = Petal.Length)) +
#'  geom_symbol(ggplot2::aes(symbol = Species)) +
#'  scale_symbol_manual(values = c(3, 1, 8))
#'

#' @rdname scale_manual_plottwist2
#' @import ggplot2
#' @export
scale_symbol_manual <- function (..., values, breaks = waiver(), na.value = NA) {
  manual_scale("symbol", values, breaks, ..., na.value = na.value)
}

#' @rdname scale_manual_plottwist2
#' @import ggplot2
#' @export
scale_pattern_manual <- function (..., values, breaks = waiver(), na.value = NA) {
  manual_scale("pattern", values, breaks, ..., na.value = na.value)
}

#' @rdname scale_manual_plottwist2
#' @import ggplot2
#' @export
scale_pattern_background_manual <- function (..., values, breaks = waiver(), na.value = NA) {
  manual_scale("pattern_background", values, breaks, ..., na.value = na.value)
}

#' @rdname scale_manual_plottwist2
#' @import ggplot2
#' @export
scale_pattern_linewidth_manual <- function (..., values, breaks = waiver(), na.value = NA) {
  manual_scale("pattern_linewidth", values, breaks, ..., na.value = na.value)
}

#' @rdname scale_manual_plottwist2
#' @import ggplot2
#' @export
scale_pattern_scale_manual <- function (..., values, breaks = waiver(), na.value = NA) {
  manual_scale("pattern_scale", values, breaks, ..., na.value = na.value)
}
