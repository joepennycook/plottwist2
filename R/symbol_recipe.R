#' Define new symbol
#'
#' Supply coordinates for a symbol to be plotted with `geom_symbol()` or
#' `geom_symbolswarm()`.
#'
#' @param x A numeric vector specifying x values.
#' @param y A numeric vector specifying y values.
#' @param id A numeric vector used to separate locations in x and y into
#' multiple lines. All locations with the same `id` belong to the same line.
#'
#' @returns An string which can be passed to `geom_symbol()` or
#' `geom_symbolswarm()` to be interpreted.
#' @export
#'
#' @examples
symbol_recipe <- function(x, y, id = NA) {
  if (is.na(id)) {
    id <- rep(1, length(x))
  }

  if (length(x) != length(y)) {
    stop("x and y are not the same length.")
  }

  if (length(x) != length(id)) {
    stop("id is not the same length as the coordinates.")
  }

  paste0("symbol_",
         paste(x, collapse = ","),
         "_",
         paste(y, collapse = ","),
         "_",
         paste(id, collapse = ","))
}

symbol_recipe_to_list <- function(recipe) {

  split_recipe <- strsplit(strsplit(recipe, "_")[[1]], ",")

  if (split_recipe[[1]] != "symbol") {
    stop("input is not a symbol recipe.")
  }

  list("x" = as.numeric(split_recipe[[2]]),
       "y" = as.numeric(split_recipe[[3]]),
       "id" = as.numeric(split_recipe[[4]]))
}

pattern_recipe <- function(x, y, id = NA,
                           solid_x = NA, solid_y = NA, solid_id = NA,
                           scale = 1, ratio = 1, invert = FALSE) {
  if (is.na(id) & !is.na(x)) {
    id <- rep(1, length(x))
  }

  if (length(x) != length(y)) {
    stop("x and y are not the same length.")
  }

  if (length(x) != length(id)) {
    stop("id is not the same length as the coordinates.")
  }

  paste0("pattern_",
         paste(x, collapse = ","),
         "_",
         paste(y, collapse = ","),
         "_",
         paste(id, collapse = ","),
         "_",
         paste(solid_x, collapse = ","),
         "_",
         paste(solid_y, collapse = ","),
         "_",
         paste(solid_id, collapse = ","),
         "_",
         paste(scale, collapse = ","),
         "_",
         paste(ratio, collapse = ","),
         "_",
         paste(invert, collapse = ","))
}

pattern_recipe_to_list <- function(recipe) {

  split_recipe <- strsplit(strsplit(recipe, "_")[[1]], ",")

  if (split_recipe[[1]] != "pattern") {
    stop("input is not a pattern recipe.")
  }

  outp <- list("x" = as.numeric(split_recipe[[2]]),
       "y" = as.numeric(split_recipe[[3]]),
       "id" = as.numeric(split_recipe[[4]]),
       "solid_x" = as.numeric(split_recipe[[5]]),
       "solid_y" = as.numeric(split_recipe[[6]]),
       "solid_id" = as.numeric(split_recipe[[7]]),
       "scale" = as.numeric(split_recipe[[8]]),
       "ratio" = as.numeric(split_recipe[[9]]),
       "invert" = as.logical(split_recipe[[10]]))

  outp[sapply(outp, length) == 0] <- NA

  outp
}
