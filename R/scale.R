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
  if (n <= 196) {
    c(c(c(11, 3, 14, 9, 4, 13,
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
    warn("More than 196 symbols requested. Consider specifying patterns manually.")
  }
}

#' @export
scale_pattern_background_discrete <- function(name = waiver(), ...,
                                              aesthetics = "pattern_background",
                                              palette = pal_pattern_background) {
  discrete_scale(aesthetics, name = name, palette = pal_pattern_background)
}

pal_pattern_background <- function(n) {
  seq(n) + 1
}

#' @import scales
#' @export
scale_pattern_background_continuous <- function (name = waiver(), breaks = waiver(), labels = waiver(),
                                                limits = NULL, range = c(1, 6), transform = "identity", trans = deprecated(),
                                                guide = "legend")
{

  continuous_scale("pattern_background", palette = pal_rescale(range),
                   name = name, breaks = breaks, labels = labels, limits = limits,
                   transform = transform, trans = trans, guide = guide)
}


#' @import scales
#' @export
scale_pattern_linewidth_continuous <- function (name = waiver(), breaks = waiver(), labels = waiver(),
                                                limits = NULL, range = c(1, 6), transform = "identity", trans = deprecated(),
                                                guide = "legend")
{

  continuous_scale("pattern_linewidth", palette = pal_rescale(range),
                   name = name, breaks = breaks, labels = labels, limits = limits,
                   transform = transform, trans = trans, guide = guide)
}

#' @export
scale_pattern_linewidth <- scale_pattern_linewidth_continuous

#' @export
scale_pattern_linewidth_ordinal <- function (name = waiver(), ..., range = c(2, 6))
{
  force(range)
  discrete_scale("pattern_linewidth", name = name, palette = function(n) seq(range[1],
                                                                     range[2], length.out = n), ...)
}

#' @import scales
#' @export
scale_pattern_scale_continuous <- function (name = waiver(), breaks = waiver(), labels = waiver(),
                                                limits = NULL, range = c(1, 6), transform = "identity", trans = deprecated(),
                                                guide = "legend")
{
  continuous_scale("pattern_scale", palette = pal_rescale(range),
                   name = name, breaks = breaks, labels = labels, limits = limits,
                   transform = transform, trans = trans, guide = guide)
}

#' @export
scale_pattern_scale <- scale_pattern_scale_continuous

#' @export
scale_pattern_scale_ordinal <- function (name = waiver(), ..., range = c(2, 6))
{
  force(range)
  discrete_scale("pattern_scale", name = name, palette = function(n) seq(range[1],
                                                                             range[2], length.out = n), ...)
}

deprecated <- function ()
{
  rlang::missing_arg()
}

# on github it looks like the way ggplot2 handles this kind of stuff is
# changing soon. I'm not going to try too hard to include binned, ordinal etc
# scales right now, and I will look into it more when things change.
