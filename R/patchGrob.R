#' @export
#' @import ggplot2
#' @import grid
#' @import rlang
patchGrob <- function(xmin = unit(0.2, "npc"),
                      xmax = unit(0.8, "npc"),
                      ymin = unit(0.2, "npc"),
                      ymax = unit(0.8, "npc"),
                      pattern_scale = 1,
                      linewidth = 1,
                      pattern_linewidth = 1,
                      pattern = 1,
                      fill = "black",
                      pattern_background = "white",
                      colour = "black",
                      name = NULL,
                      gp = NULL,
                      vp = NULL) {

  grob(xmin = xmin,
       xmax = xmax,
       ymin = ymin,
       ymax = ymax,
       pattern = pattern,
       pattern_scale = pattern_scale,
       linewidth = linewidth,
       pattern_linewidth = pattern_linewidth,
       fill = fill,
       pattern_background = pattern_background,
       colour = colour,
       name = name,
       gp = gp,
       vp = vp,
       cl = "patch")
}

#' @export
drawDetails.patch <- function(x, ...) {
  pattern_details <- pattern_lookup[[x$pattern]]

  # sort out pattern aesthetic details
  pattern <- x$pattern

  # extract symbol details according to data type
  if (class(pattern) != "character") {
    pattern_details <- pattern_lookup[[pattern]]
  } else if (grepl("pattern_", pattern)) {
    pattern_details <- pattern_recipe_to_list(pattern)
  } else if (pattern %in% names(pattern_lookup)) {
    pattern_details <- pattern_lookup[[pattern]]
  } else {
    stop("Pattern values not recognised. Consider supplying data to the pattern aesthetic as a factor.")
  }

  # bounds of patch
  mm_xmin <- convertX(x$xmin, "mm", valueOnly = TRUE)
  mm_xmax <- convertX(x$xmax, "mm", valueOnly = TRUE)
  mm_ymin <- convertY(x$ymin, "mm", valueOnly = TRUE)
  mm_ymax <- convertY(x$ymax, "mm", valueOnly = TRUE)

  # fill colour
  primary_fill <- ifelse(pattern_details$invert,
                         x$pattern_background, x$fill)
  secondary_fill <- ifelse(pattern_details$invert,
                         x$fill, x$pattern_background)

  # convert from xmin etc. to the relevant polygon coordinates
  polymap <- data.frame(
    "y" = c(mm_ymax, mm_ymax, mm_ymin, mm_ymin, mm_ymax, mm_ymax),
    "x" = c(mm_xmin, mm_xmax, mm_xmax, mm_xmin, mm_xmin, mm_xmax)
  )

  # create the base rectangle graphical object according to parameters
  grid.polygon(x = polymap$x,
               y = polymap$y,
               default.units = "mm",
               gp = gpar(fill = secondary_fill,
                         col = x$colour,
                         lwd = x$linewidth))

  # figure out the size of each tile and how many fit on each patch
  pattern_scale <- x$pattern_scale * 1.875
  tile_height <- pattern_scale * pattern_details$scale
  tile_width <- pattern_scale * pattern_details$scale * pattern_details$ratio

  tiles_wide <- (mm_xmax - mm_xmin) / tile_width
  tiles_high <- (mm_ymax - mm_ymin) / tile_height

  draw_solid <- (!is.na(pattern_details[[4]][1]))
  draw_line <- (!is.na(pattern_details[[1]][1]) &
                  (!draw_solid | FALSE))

  # if I want to replace 'fill_outline' as an aesthetic to decide if the filled
  # shapes also have lines, replace 'TRUE' above with 'x$fill_outline == TRUE'
  # then of course replace 'fill_outline' in the normal places

  if(draw_solid) {
    # extrapolate solid coordinates across the size of the patch
    solid_data <- as.data.frame(pattern_details[4:6])
    colnames(solid_data) <- c("x", "y", "id")
    solid_coords <- assemble_patch(solid_data,
                                   tiles_wide, tiles_high)
    solid_coords$x <- (solid_coords$x * tile_width) + mm_xmin
    solid_coords$y <- (solid_coords$y * tile_height) + mm_ymin

    # draw solids
    grid.polygon(x = solid_coords$x,
                 y = solid_coords$y,
                 id = solid_coords$id,
                 default.units = "mm",
                 gp = gpar(col = "#00000000",
                           fill = primary_fill,
                           lwd = x$pattern_linewidth))
  }

  if(draw_line) {
    # extrapolate pattern coordinates across the size of the patch
    pattern_coords <- assemble_patch(as.data.frame(pattern_details[1:3]),
                                     tiles_wide, tiles_high)
    pattern_coords$x <- (pattern_coords$x * tile_width) + mm_xmin
    pattern_coords$y <- (pattern_coords$y * tile_height) + mm_ymin

    # draw pattern
    grid.polyline(x = pattern_coords$x,
                  y = pattern_coords$y,
                  id = pattern_coords$id,
                  default.units = "mm",
                  gp = gpar(col = primary_fill,
                            lwd = x$pattern_linewidth))
  }

  # draw border
  grid.polygon(x = polymap$x,
               y = polymap$y,
               default.units = "mm",
               gp = gpar(fill = "#00000000",
                         col = x$colour,
                         lwd = x$linewidth))
}

calc_new_to <- function(foc_from,
                        foc_old_to,
                        alt_from,
                        alt_old_to,
                        alt_new_to) {

  foc_from + (foc_old_to - foc_from) * prop_dist(alt_from,
                                                 alt_old_to,
                                                 alt_new_to)
}

prop_dist <- function(from, old_to, new_to) {
  (new_to - from) / (old_to - from)
}

assemble_patch <- function(tile, width, height) {

  wide_round <- floor(width)
  wide_fraction <- width - wide_round
  high_round <- floor(height)
  high_fraction <- height - high_round

  ## map out all the whole tiles
  # everything except right column (rc) and top row (tr)
  main_body <- lay_tiles(tile, wide_round, high_round)

  id_max <- 0
  id_max <- ifelse(nrow(main_body) > 0,
                   suppressWarnings(max(main_body$id)),
                   max(id_max))

  # triplicate rows which will overlap a border on rc or tr
  # we sometimes need the first to be the point crossing out of the patch
  # we sometimes need the last to be the point crossing back in
  # we sometimes need the middle to define the corner of the patch
  tile_rep <- tile[rep(seq(nrow(tile)),
                       ifelse(tile$x > wide_fraction |
                                tile$y > high_fraction,
                              3, 2)), ]
  rep_n <- nrow(tile_rep)

  ## map out the right column of tiles
  # sort out which coordinates cross over the boundary
  x_wide <- tile_rep$x > wide_fraction

  x_out_forw <- c(FALSE, x_wide[-1] & !(x_wide[-rep_n]) &
                    (tile_rep$id[-1] == tile_rep$id[-rep_n]))
  x_in_forw <- c(x_out_forw[-1], FALSE)

  x_out_back <- c(x_wide[-rep_n] & !(x_wide[-1]) &
                    (tile_rep$id[-1] == tile_rep$id[-rep_n]), FALSE)
  x_in_back <- c(FALSE, x_out_back[-rep_n])

  x_out <- x_out_forw | x_out_back
  x_in <- x_in_forw | x_in_back

  # calculate the intersection with the boundary for crossing lines
  # and stop the line there
  rc_tile <- tile_rep

  rc_tile$x[x_out] <- wide_fraction
  rc_tile$y[x_out] <- calc_new_to(foc_from = tile_rep$y[x_in],
                                  foc_old_to = tile_rep$y[x_out],
                                  alt_from = tile_rep$x[x_in],
                                  alt_old_to = tile_rep$x[x_out],
                                  alt_new_to = wide_fraction)

  rc_tile_trim <- rc_tile[!(x_wide & !x_out), ]

  # repeat new coordinates to fill the right column
  right_column <- lay_tiles(rc_tile_trim, 1, high_round)
  right_column$x <- right_column$x + wide_round
  right_column$id <- right_column$id + id_max
  id_max <- ifelse(nrow(right_column) > 0,
                   suppressWarnings(max(right_column$id)),
                   max(id_max))

  ## map out the top row of tiles
  # sort out which coordinates cross over the boundary
  y_high <- tile_rep$y > high_fraction

  y_out_forw <- c(FALSE, y_high[-1] & !(y_high[-rep_n]) &
                    (tile_rep$id[-1] == tile_rep$id[-rep_n]))
  y_in_forw <- c(y_out_forw[-1], FALSE)

  y_out_back <- c(y_high[-rep_n] & !(y_high[-1]) &
                    (tile_rep$id[-1] == tile_rep$id[-rep_n]), FALSE)
  y_in_back <- c(FALSE, y_out_back[-rep_n])

  y_out <- y_out_forw | y_out_back
  y_in <- y_in_forw | y_in_back

  # calculate the intersection with the boundary for crossing lines
  # and stop the line there
  tr_tile <- tile_rep

  tr_tile$y[y_out] <- high_fraction
  tr_tile$x[y_out] <- calc_new_to(foc_from = tile_rep$x[y_in],
                                  foc_old_to = tile_rep$x[y_out],
                                  alt_from = tile_rep$y[y_in],
                                  alt_old_to = tile_rep$y[y_out],
                                  alt_new_to = high_fraction)

  tr_tile_trim <- tr_tile[!(y_high & !y_out), ]

  # repeat new coordinates to fill the top row
  top_row <- lay_tiles(tr_tile_trim, wide_round, 1)
  top_row$y <- top_row$y + high_round
  top_row$id <- top_row$id + id_max
  id_max <- ifelse(nrow(top_row) > 0,
                   suppressWarnings(max(top_row$id)),
                   max(id_max))

  # map out the top right corner tile
  # sort out which coordinates cross directly over both boundaries
  xy_wide_high <- x_wide & y_high
  xy_interior <- !(x_wide | y_high)

  xy_out_forw <- c(FALSE, xy_wide_high[-1] & xy_interior[-rep_n] &
                     (tile_rep$id[-1] == tile_rep$id[-rep_n]))
  xy_in_forw <- c(xy_out_forw[-1], FALSE)

  xy_out_back <- c(xy_wide_high[-rep_n] & xy_interior[-1] &
                     (tile_rep$id[-1] == tile_rep$id[-rep_n]), FALSE)
  xy_in_back <- c(FALSE, xy_out_back[-rep_n])

  xy_out <- xy_out_forw | xy_out_back
  xy_in <- xy_in_forw | xy_in_back

  trc_tile <- tile_rep

  # copy previously calculated coordinates for single border crosses
  trc_tile[x_out, ] <- rc_tile[x_out, ]
  trc_tile[y_out, ] <- tr_tile[y_out, ]

  # calculate whether xy crosses intersect with x or y first
  xy_dist <- prop_dist(from = tile_rep$x[xy_in],
                       old_to = tile_rep$x[xy_out],
                       new_to = wide_fraction) <
    prop_dist(from = tile_rep$y[xy_in],
              old_to = tile_rep$y[xy_out],
              new_to = high_fraction)

  # depending on first boundary crossed, calculate new coordinates
  trc_tile$x[xy_out] <-
    ifelse(xy_dist,
           wide_fraction,
           calc_new_to(foc_from = tile_rep$x[xy_in],
                       foc_old_to = tile_rep$x[xy_out],
                       alt_from = tile_rep$y[xy_in],
                       alt_old_to = tile_rep$y[xy_out],
                       alt_new_to = high_fraction)
    )

  trc_tile$y[xy_out] <-
    ifelse(xy_dist,
           calc_new_to(foc_from = tile_rep$y[xy_in],
                       foc_old_to = tile_rep$y[xy_out],
                       alt_from = tile_rep$x[xy_in],
                       alt_old_to = tile_rep$x[xy_out],
                       alt_new_to = wide_fraction),
           high_fraction
    )

  # fill in corners in case the line crosses out then back in
  corners <- which(x_out_forw | y_out_forw | xy_out_forw) + 1
  trc_tile$x[corners] <- wide_fraction
  trc_tile$y[corners] <- high_fraction

  trc_tile_trim <- trc_tile[!(trc_tile$x > wide_fraction |
                                trc_tile$y > high_fraction), ]

  # update final details for top right corner
  top_right_corner <- trc_tile_trim
  top_right_corner$x <- top_right_corner$x + wide_round
  top_right_corner$y <- top_right_corner$y + high_round
  top_right_corner$id <- top_right_corner$id + id_max

  # combine into a single data frame
  rbind(main_body,
        right_column,
        top_row,
        top_right_corner)

}

lay_tiles <- function(tile, width, height) {
  n_tiles <- width * height
  max_id <- suppressWarnings(max(tile$id))

  x_index <- rep(rep(seq(width) - 1,
                     each = nrow(tile)),
                 times = height)
  y_index <- rep(seq(height) - 1,
                 each = nrow(tile) * width)

  data.frame("x" = rep(tile$x, n_tiles) + x_index,
             "y" = rep(tile$y, n_tiles) + y_index,
             "id" = rep(tile$id, n_tiles) +
               x_index * max_id +
               y_index * width * max_id
  )
}
