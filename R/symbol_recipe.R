#' Define new symbol
#'
#' Supply coordinates for a symbol to be plotted with `geom_symbol()` or
#' `geom_symbolswarm()`.
#'
#' @param x A numeric vector specifying x values.
#' @param y A numeric vector specifying y values.
#' @param id A numeric vector used to seperate locations in x and y into
#' multiple lines. All locations with the same `id` belong to the same line.
#'
#' @returns An object with class `symbol_recipe`.
#' @export
#'
#' @examples
symbol_recipe <- function(x, y, id = NULL) {
  if (is.null(id)) {
    id <- rep(1, length(x))
  }

  if (length(x) != length(y)) {
    stop("x and y are not the same length.")
  }

  if (length(x) != length(id)) {
    stop("id is not the same length as the coordinates.")
  }

  output <- list("x" = x, "y" = y, "id" = id)
  class(output) <- "symbol_recipe"
  output
}

symbol_recipe_to_string <- function(recipe) {
  if (class(recipe) != "symbol_recipe") {
    stop("The supplied object is not a symbol recipe.
         Use symbol_recipe() to define a new symbol.")
  }

  paste0(paste(recipe$x, collapse = ","),
         "_",
         paste(recipe$y, collapse = ","),
         "_",
         paste(recipe$id, collapse = ","))
}

string_to_symbol_recipe <- function(string) {
  if (class(string) != "character") {
    stop("input is not a string.")
  }

  output <- strsplit(strsplit(string, "_")[[1]], ",")

  symbol_recipe(output[[1]],
                output[[2]],
                output[[3]])
}
