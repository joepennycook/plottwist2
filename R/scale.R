#' @import ggplot2
#' @export
scale_symbol <- function(name = waiver(), ..., aesthetics = "symbol") {
  discrete_scale(aesthetics, name = name, palette = pal_symbol)
}

#' @export
scale_symbol_discrete <- scale_symbol

pal_symbol <- function(n) {
  if (n <= 3) {
    c(12, 9, 16)[seq_len(n)]
  } else if (n <= 6) {
    c(12, 20, 9, 17, 16,
      24)[seq_len(n)]
  } else if (n <= 9) {
    c(12, 20, 36, 9, 17,
      33, 16, 24, 40)[seq_len(n)]
  } else if (n <= 15) {
    c(12, 20, 36, 9, 17,
      33, 11, 19, 35, 10,
      18, 34, 16, 24, 40)[seq_len(n)]
  } else if (n <= 24) {
    c(12, 20, 28, 36, 9,
      17, 25, 33, 11, 19,
      27, 35, 10, 18, 26,
      34, 14, 22, 30, 38,
      16, 24, 32, 40)[seq_len(n)]
  } else {
    warn("More than 24 symbols requested. Consider specifying symbols manually.")
  }
}
