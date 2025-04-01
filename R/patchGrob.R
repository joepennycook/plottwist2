#' @export
#' @import ggplot2
#' @import grid
#' @import rlang
patchGrob <- function(xmin = unit(0.2, "npc"),
                      xmax = unit(0.8, "npc"),
                      ymin = unit(0.2, "npc"),
                      ymax = unit(0.8, "npc"),
                      size = 3,
                      linewidth = 1,
                      linewidth2 = 1,
                      pattern = 1,
                      fill = "black",
                      fill2 = "white",
                      colour = "black",
                      name = NULL,
                      gp = NULL,
                      vp = NULL) {

  grob(xmin = xmin,
       xmax = xmax,
       ymin = ymin,
       ymax = ymax,
       pattern = pattern,
       size = size,
       linewidth = linewidth,
       linewidth2 = linewidth2,
       fill = fill,
       fill2 = fill2,
       colour = colour,
       name = name,
       gp = gp,
       vp = vp,
       cl = "patch")
}

#' @export
drawDetails.patch <- function(x, ...) {
  pattern_details <- pattern_lookup[[x$pattern]]

  mm_xmin <- convertX(x$xmin, "mm", valueOnly = TRUE)
  mm_xmax <- convertX(x$xmax, "mm", valueOnly = TRUE)
  mm_ymin <- convertY(x$ymin, "mm", valueOnly = TRUE)
  mm_ymax <- convertY(x$ymax, "mm", valueOnly = TRUE)

  tile_height <- x$size * pattern_details$scale
  tile_width <- x$size * pattern_details$scale * pattern_details$ratio

  coord_data <- as.data.frame(pattern_details[1:3]) #
#
#   coord_data$x <- coord_data$x * tile_width #
#   coord_data$y <- coord_data$y * tile_height #

  primary_fill <- x$fill
  secondary_fill <- x$fill2

  if(pattern_details$invert) {
    primary_fill <- x$fill2
    secondary_fill <- x$fill
  }

  # convert from xmin etc. to the relevant polygon coordinates
  polymap <- rect_to_poly(mm_xmin,
                          mm_xmax,
                          mm_ymin,
                          mm_ymax)

  # create the base rectangle graphical object according to parameters
  grid.polygon(x = polymap$x,
               y = polymap$y,
               default.units = "mm",
               gp = gpar(fill = secondary_fill,
                         colour = x$colour,
                         linewidth = x$linewidth))

  # count how many tiles fit wide,
  tiles_wide <- (mm_xmax - mm_xmin) / tile_width
  # tiles_wide_round <- floor(tiles_wide)
  # tiles_wide_remainder <- (tiles_wide - tiles_wide_round) * tile_width

  # count how many tiles fit high
  tiles_high <- (mm_ymax - mm_ymin) / tile_height
  # tiles_high_round <- floor(tiles_high)
  # tiles_high_remainder <- (tiles_high - tiles_high_round) * tile_height

  pattern_coords <- assemble_patch(coord_data, tiles_wide, tiles_high)
  pattern_coords$x <- (pattern_coords$x * tile_width) + mm_xmin
  pattern_coords$y <- (pattern_coords$y * tile_height) + mm_ymin

  grid.polyline(x = pattern_coords$x,
                y = pattern_coords$y,
                id = pattern_coords$id,
                default.units = "mm",
                gp = gpar(col = primary_fill,
                          lwd = x$linewidth2))

  # how many seperate lines
  # n_id <- length(unique(coord_data$id))
  # n_points <- nrow(coord_data)
  #
  # tiles_wide_seq <- seq(tiles_wide_round) - 1
  # tiles_high_seq <- seq(tiles_high_round) - 1

  # extrapolate the tile coordinates to the main body of the patch
  # main_coord_data <- data.frame("x" = rep(coord_data$x,
  #                                         tiles_high_round * tiles_wide_round) +
  #                                 mm_xmin +
  #                                 rep(rep(tiles_wide_seq,
  #                                         each = n_points),
  #                                     times = tiles_high_round) * tile_width,
  #                               "y" = rep(coord_data$y, tiles_high_round *
  #                                           tiles_wide_round) +
  #                                 mm_ymin +
  #                                 rep(tiles_high_seq,
  #                                     each = n_points *
  #                                       tiles_wide_round) *
  #                                 tile_height,
  #                               "id" = rep(coord_data$id,
  #                                          tiles_high_round *
  #                                            tiles_wide_round) +
  #                                 (rep(rep(tiles_wide_seq,
  #                                          each = n_points),
  #                                      times = tiles_high_round) * n_id) +
  #                                 (rep(tiles_high_seq,
  #                                      each = n_points *
  #                                        tiles_wide_round) *
  #                                    tiles_wide_round * n_id))
  #
  # x_out <- coord_data$x > tiles_wide_remainder
  # y_out <- coord_data$y > tiles_high_remainder
  #
  # # create copy of coord_data with doubled overlapping points
  # # to help with points which are both crossing out and crossing in later
  # dup_coord_data <- coord_data[rep(seq(nrow(coord_data)),
  #                                  ifelse(x_out | y_out, 2, 1)), ]
  #
  # dup_x_out <- dup_coord_data$x > tiles_wide_remainder
  # dup_y_out <- dup_coord_data$y > tiles_high_remainder
  #
  # dup_n_points <- nrow(dup_coord_data)
  #
  # # right column
  #
  # # find coordinates which are outside but come from inside
  # x_cross_out <- c(FALSE, dup_x_out[-1] & !(dup_x_out[-dup_n_points]) &
  #                    (dup_coord_data$id[-1] == dup_coord_data$id[-dup_n_points]))
  # x_cross_from <- c(x_cross_out[-1], FALSE)
  #
  # # find coordinates which are outside but are going inside
  # x_cross_in <- c(dup_x_out[-dup_n_points] & !(dup_x_out[-1]) &
  #                   (dup_coord_data$id[-1] == dup_coord_data$id[-dup_n_points]),
  #                 FALSE)
  # x_cross_to <- c(FALSE, x_cross_in[-dup_n_points])
  #
  # rc_coord_data <- dup_coord_data
  #
  # rc_coord_data$x[x_cross_out] <- tiles_wide_remainder
  # rc_coord_data$y[x_cross_out] <- calc_new_to(foc_from = dup_coord_data$y[x_cross_from],
  #                                             foc_old_to = dup_coord_data$y[x_cross_out],
  #                                             alt_from = dup_coord_data$x[x_cross_from],
  #                                             alt_old_to = dup_coord_data$x[x_cross_out],
  #                                             alt_new_to = tiles_wide_remainder)
  #
  # rc_coord_data$x[x_cross_in] <- tiles_wide_remainder
  # rc_coord_data$y[x_cross_in] <- calc_new_to(foc_from = dup_coord_data$y[x_cross_to],
  #                                            foc_old_to = dup_coord_data$y[x_cross_in],
  #                                            alt_from = dup_coord_data$x[x_cross_to],
  #                                            alt_old_to = dup_coord_data$x[x_cross_in],
  #                                            alt_new_to = tiles_wide_remainder)
  # # remove points which are outside but not adjacent to one inside
  # trim_rc_coord_data <- rc_coord_data[!(dup_x_out & !x_cross_out & !x_cross_in), ]
  #
  # right_column_coord_data <- data.frame("x" = rep(trim_rc_coord_data$x,
  #                                                 tiles_high_round) +
  #                                         mm_xmin +
  #                                         tiles_wide_round * tile_width,
  #                                       "y" = rep(trim_rc_coord_data$y,
  #                                                 tiles_high_round) +
  #                                         mm_ymin +
  #                                         rep(tiles_high_seq,
  #                                             each = nrow(trim_rc_coord_data)) *
  #                                         tile_height,
  #                                       "id" = rep(trim_rc_coord_data$id,
  #                                                  tiles_high_round) +
  #                                         # then add a displacement based on x tiling
  #                                         (rep(tiles_high_seq,
  #                                              each = nrow(trim_rc_coord_data)) *
  #                                            n_id) +
  #                                         max(main_coord_data$id))
  #
  # full_coord_data <- rbind(main_coord_data,
  #                          right_column_coord_data)
  #
  # # top row
  #
  # y_cross_out <- c(FALSE, dup_y_out[-1] & !(dup_y_out[-dup_n_points]) &
  #                    (dup_coord_data$id[-1] == dup_coord_data$id[-dup_n_points]))
  # y_cross_from <- c(y_cross_out[-1], FALSE)
  #
  # # find coordinates which are outside but are going inside
  # y_cross_in <- c(dup_y_out[-dup_n_points] & !(dup_y_out[-1]) &
  #                   (dup_coord_data$id[-1] == dup_coord_data$id[-dup_n_points]),
  #                 FALSE)
  # y_cross_to <- c(FALSE, y_cross_in[-dup_n_points])
  #
  # tr_coord_data <- dup_coord_data
  #
  # tr_coord_data$y[y_cross_out] <- tiles_high_remainder
  # tr_coord_data$x[y_cross_out] <- calc_new_to(foc_from = dup_coord_data$x[y_cross_from],
  #                                             foc_old_to = dup_coord_data$x[y_cross_out],
  #                                             alt_from = dup_coord_data$y[y_cross_from],
  #                                             alt_old_to = dup_coord_data$y[y_cross_out],
  #                                             alt_new_to = tiles_high_remainder)
  #
  # tr_coord_data$y[y_cross_in] <- tiles_high_remainder
  # tr_coord_data$x[y_cross_in] <- calc_new_to(foc_from = dup_coord_data$x[y_cross_to],
  #                                            foc_old_to = dup_coord_data$x[y_cross_in],
  #                                            alt_from = dup_coord_data$y[y_cross_to],
  #                                            alt_old_to = dup_coord_data$y[y_cross_in],
  #                                            alt_new_to = tiles_high_remainder)
  #
  # # remove points which are outside but not adjacent to one inside
  # trim_tr_coord_data <- tr_coord_data[!(dup_y_out & !y_cross_out & !y_cross_in), ]
  #
  # top_row_coord_data <- data.frame("x" = rep(trim_tr_coord_data$x,
  #                                            tiles_wide_round) +
  #                                    mm_xmin +
  #                                    rep(tiles_wide_seq,
  #                                        each = nrow(trim_tr_coord_data)) *
  #                                    tile_width,
  #                                  "y" = rep(trim_tr_coord_data$y,
  #                                            tiles_wide_round) +
  #                                    mm_ymin +
  #                                    tiles_high_round * tile_height,
  #                                  "id" = rep(trim_tr_coord_data$id,
  #                                             tiles_wide_round) +
  #                                    (rep(tiles_wide_seq,
  #                                         each = nrow(trim_tr_coord_data)) *
  #                                       n_id) +
  #                                    max(full_coord_data$id))
  #
  # full_coord_data <- rbind(full_coord_data,
  #                          top_row_coord_data)
  #
  # # calculate coordinates for the top right corner overlap
  #
  # xy_out <- dup_x_out & dup_y_out
  # xy_in <- !(dup_x_out | dup_y_out)
  #
  # xy_cross_out <- c(FALSE, xy_out[-1] & xy_in[-dup_n_points] &
  #                     (dup_coord_data$id[-1] == dup_coord_data$id[-dup_n_points]))
  # xy_cross_from <- c(xy_cross_out[-1], FALSE)
  #
  # # find coordinates which are outside but are going inside
  # xy_cross_in <- c(xy_out[-dup_n_points] & xy_in[-1] &
  #                    (dup_coord_data$id[-1] == dup_coord_data$id[-dup_n_points]),
  #                  FALSE)
  # xy_cross_to <- c(FALSE, xy_cross_in[-dup_n_points])
  #
  # trc_coord_data <- dup_coord_data
  #
  # # copy in the rows which only cross one boundary
  # # remember to trim the easy ones later
  # trc_coord_data[x_cross_out | x_cross_in, ] <-
  #   rc_coord_data[x_cross_out | x_cross_in, ]
  #
  # trc_coord_data[y_cross_out | y_cross_in, ] <-
  #   tr_coord_data[y_cross_out | y_cross_in, ]
  #
  # out_xy_dist <- prop_dist(from = dup_coord_data$x[xy_cross_from],
  #                          old_to = dup_coord_data$x[xy_cross_out],
  #                          new_to = tiles_wide_remainder) <
  #   prop_dist(from = dup_coord_data$y[xy_cross_from],
  #             old_to = dup_coord_data$y[xy_cross_out],
  #             new_to = tiles_high_remainder)
  #
  # # if it crosses x boundary first
  # # x is the just the boundary
  # # otherwise calculate it
  # trc_coord_data$x[xy_cross_out] <-
  #   ifelse(out_xy_dist,
  #          tiles_wide_remainder,
  #          calc_new_to(foc_from = dup_coord_data$x[xy_cross_from],
  #                      foc_old_to = dup_coord_data$x[xy_cross_out],
  #                      alt_from = dup_coord_data$y[xy_cross_from],
  #                      alt_old_to = dup_coord_data$y[xy_cross_out],
  #                      alt_new_to = tiles_high_remainder)
  #   )
  #
  # # if it crosses the x boundary first
  # # calculate y
  # # otherwise y is the boundary
  #
  # trc_coord_data$y[xy_cross_out] <-
  #   ifelse(out_xy_dist,
  #          calc_new_to(foc_from = dup_coord_data$y[xy_cross_from],
  #                      foc_old_to = dup_coord_data$y[xy_cross_out],
  #                      alt_from = dup_coord_data$x[xy_cross_from],
  #                      alt_old_to = dup_coord_data$x[xy_cross_out],
  #                      alt_new_to = tiles_wide_remainder),
  #          tiles_high_remainder
  #   )
  #
  # # if it crosses x boundary first
  # # x is the just the boundary
  # # otherwise calculate it
  #
  # in_xy_dist <- prop_dist(from = dup_coord_data$x[xy_cross_to],
  #                         old_to = dup_coord_data$x[xy_cross_in],
  #                         new_to = tiles_wide_remainder) <
  #   prop_dist(from = dup_coord_data$y[xy_cross_to],
  #             old_to = dup_coord_data$y[xy_cross_in],
  #             new_to = tiles_high_remainder)
  #
  # trc_coord_data$x[xy_cross_in] <-
  #   ifelse(in_xy_dist,
  #          tiles_wide_remainder,
  #          calc_new_to(foc_from = dup_coord_data$x[xy_cross_to],
  #                      foc_old_to = dup_coord_data$x[xy_cross_in],
  #                      alt_from = dup_coord_data$y[xy_cross_to],
  #                      alt_old_to = dup_coord_data$y[xy_cross_in],
  #                      alt_new_to = tiles_high_remainder)
  #   )
  #
  # # if it crosses the x boundary first
  # # calculate y
  # # otherwise y is the boundary
  # trc_coord_data$y[xy_cross_in] <-
  #   ifelse(in_xy_dist,
  #          calc_new_to(foc_from = dup_coord_data$y[xy_cross_to],
  #                      foc_old_to = dup_coord_data$y[xy_cross_in],
  #                      alt_from = dup_coord_data$x[xy_cross_to],
  #                      alt_old_to = dup_coord_data$x[xy_cross_in],
  #                      alt_new_to = tiles_wide_remainder),
  #          tiles_high_remainder
  #   )
  #
  # trc_coord_data[x_cross_out | y_cross_out | xy_cross_out, ]
  # rep(seq(nrow(trc_coord_data)),
  #     ifelse(x_cross_out | y_cross_out | xy_cross_out,
  #            2, 1))
  #
  # any_out <- x_cross_out |
  #         y_cross_out |
  #         xy_cross_out
  #
  # dup_trc_coord_data <- trc_coord_data[rep(seq(nrow(trc_coord_data)),
  #                                          ifelse(any_out,
  #                                                 2, 1)), ]
  #
  # extra_corners <- (which(any_out) + seq(from = 0,
  #                                        to = sum(any_out) - 1)) + 1
  #
  # dup_trc_coord_data$x[extra_corners] <- tiles_wide_remainder
  # dup_trc_coord_data$y[extra_corners] <- tiles_high_remainder
  #
  # # remove points which are outside but not adjacent to one inside
  # trim_trc_coord_data <- dup_trc_coord_data[!(dup_trc_coord_data$x >
  #                                           tiles_wide_remainder |
  #                                             dup_trc_coord_data$y >
  #                                           tiles_high_remainder), ]
  #
  # top_right_corner_coord_data <- trim_trc_coord_data
  #
  # top_right_corner_coord_data$x <- trim_trc_coord_data$x + mm_xmin +
  #   tiles_wide_round * tile_width
  # top_right_corner_coord_data$y <- trim_trc_coord_data$y + mm_ymin +
  #   tiles_high_round * tile_height
  #
  # top_right_corner_coord_data$id <- trim_trc_coord_data$id + max(full_coord_data$id)
  #
  # # I'm probably leaving in a bug where the line will cut across the top right corner in some circumstances
  # # I'd like to split apart the id of any cases where this would be an issue
  # # I think it's situations where a line crosses over the x then back over the y
  # # or vice versa
  # # best just to split the ids of any adjacent outs and backs
  #
  # # also with hex I see an overlap in the top right
  # # something about my code to figure out which axis is being crossed isn't working
  # # might be a perpendicular line issue?
  #
  # full_coord_data <- rbind(full_coord_data,
  #                          top_right_corner_coord_data)
  #
  # grid.polyline(x = full_coord_data$x,
  #               y = full_coord_data$y,
  #               id = full_coord_data$id,
  #               default.units = "mm",
  #               gp = gpar(col = primary_fill,
  #                         lwd = x$linewidth2))

  # draw a final outline around the patch

  grid.polygon(x = polymap$x,
               y = polymap$y,
               default.units = "mm",
               gp = gpar(fill = "#00000000",
                         colour = x$colour,
                         linewidth = x$linewidth))
}

data_frame0 <- function(...) vctrs::data_frame(..., .name_repair = "minimal")

rect_to_poly <- function(xmin, xmax, ymin, ymax) {
  data_frame0(
    y = c(ymax, ymax, ymin, ymin, ymax, ymax),
    x = c(xmin, xmax, xmax, xmin, xmin, xmax)
  )
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
  # excess will be trimmed
  tile_rep <- tile[rep(seq(nrow(tile)),
                       ifelse(tile$x > wide_fraction |
                                tile$y > high_fraction,
                              3, 1)), ]
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
  max_id <- max(tile$id)

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
