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
    warn("More than 24 symbols required. Consider specifying symbols manually.")
  }
}

#' @import ggplot2
#' @export
scale_pattern <- function(name = waiver(), ..., aesthetics = "pattern") {
  discrete_scale(aesthetics, name = name, palette = pal_pattern)
}

#' @export
scale_pattern_discrete <- scale_pattern

pal_pattern <- function(n) {
  if (n <= 282) {
    c(c(c(1, 11, 3, 14, 9, 4, 13,
          38, 5, 24, 27, 39, 8, 20, 25,
          12, 2, 16, 34, 18, 32, 35, 22,
          28, 40, 15, 19, 30, 37,
          51, 44, 54, 49, 45, 53,
          46, 64, 47, 60, 52, 42, 56,
          58, 62, 55, 59),
        c(11, 3, 14, 9, 4, 13,
          38, 5, 24, 27, 39, 8, 20, 25,
          12, 2, 16, 34, 18, 32, 35, 22,
          28, 40, 15, 19, 30, 37,
          51, 44, 54, 49, 45, 53,
          46, 64, 47, 60, 52, 42, 56,
          58, 62, 55, 59) + 64,
        c(11, 3, 14, 9, 4, 13,
          38, 5, 24, 27, 39, 8, 20, 25,
          12, 2, 16, 34, 18, 32, 35, 22,
          28, 40, 15, 19, 30, 37,
          51, 44, 54, 49, 45, 53,
          46, 64, 47, 60, 52, 42, 56,
          58, 62, 55, 59) + 128),
      c(c(38, 24, 27, 39, 8, 20, 25,
          12, 34, 18, 32, 35, 22,
          28, 40, 19, 30, 37),
        c(38, 24, 27, 39, 8, 20, 25,
          12, 34, 18, 32, 35, 22,
          28, 40, 19, 30, 37) + 64,
        c(11, 3, 14, 9, 13,
          38, 24, 27, 39, 8, 20, 25,
          12, 16, 34, 18, 32, 35, 22,
          28, 40, 15, 19, 30, 37) + 128) + 192)
  } else {
    warn("More than 197 symbols requested. Consider specifying patterns manually.")
  }
}
