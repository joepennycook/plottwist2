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

  coord_data <- as.data.frame(pattern_details[1:3])
  coord_data$x <- coord_data$x * tile_width
  coord_data$y <- coord_data$y * tile_height

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
  tiles_wide_round <- floor(tiles_wide)
  tiles_wide_remainder <- (tiles_wide - tiles_wide_round) * tile_width

  # count how many tiles fit high
  tiles_high <- (mm_ymax - mm_ymin) / tile_height
  tiles_high_round <- floor(tiles_high)
  tiles_high_remainder <- (tiles_high - tiles_high_round) * tile_height

  # how many seperate lines
  n_id <- length(unique(coord_data$id))
  n_points <- nrow(coord_data)

  tiles_wide_seq <- seq(tiles_wide_round) - 1
  tiles_high_seq <- seq(tiles_high_round) - 1

  # extrapolate the tile coordinates to the main body of the patch
  main_coord_data <- data.frame("x" = rep(coord_data$x,
                                          tiles_high_round * tiles_wide_round) +
                                  mm_xmin +
                                  rep(rep(tiles_wide_seq,
                                          each = n_points),
                                      times = tiles_high_round) * tile_width,
                                "y" = rep(coord_data$y, tiles_high_round *
                                            tiles_wide_round) +
                                  mm_ymin +
                                  rep(tiles_high_seq,
                                      each = n_points *
                                        tiles_wide_round) *
                                  tile_height,
                                "id" = rep(coord_data$id,
                                           tiles_high_round *
                                             tiles_wide_round) +
                                  (rep(rep(tiles_wide_seq,
                                           each = n_points),
                                       times = tiles_high_round) * n_id) +
                                  (rep(tiles_high_seq,
                                       each = n_points *
                                         tiles_wide_round) *
                                     tiles_wide_round * n_id))

  x_out <- coord_data$x > tiles_wide_remainder
  y_out <- coord_data$y > tiles_high_remainder

  # create copy of coord_data with doubled overlapping points
  # to help with points which are both crossing out and crossing in later
  dup_coord_data <- coord_data[rep(seq(nrow(coord_data)),
                                   ifelse(x_out | y_out, 2, 1)), ]

  dup_x_out <- dup_coord_data$x > tiles_wide_remainder
  dup_y_out <- dup_coord_data$y > tiles_high_remainder

  dup_n_points <- nrow(dup_coord_data)

  # right column

  # find coordinates which are outside but come from inside
  x_cross_out <- c(FALSE, dup_x_out[-1] & !(dup_x_out[-dup_n_points]) &
                     (dup_coord_data$id[-1] == dup_coord_data$id[-dup_n_points]))
  x_cross_from <- c(x_cross_out[-1], FALSE)

  # find coordinates which are outside but are going inside
  x_cross_in <- c(dup_x_out[-dup_n_points] & !(dup_x_out[-1]) &
                    (dup_coord_data$id[-1] == dup_coord_data$id[-dup_n_points]),
                  FALSE)
  x_cross_to <- c(FALSE, x_cross_in[-dup_n_points])

  rc_coord_data <- dup_coord_data

  rc_coord_data$x[x_cross_out] <- tiles_wide_remainder
  rc_coord_data$y[x_cross_out] <- calc_new_to(foc_from = dup_coord_data$y[x_cross_from],
                                              foc_old_to = dup_coord_data$y[x_cross_out],
                                              alt_from = dup_coord_data$x[x_cross_from],
                                              alt_old_to = dup_coord_data$x[x_cross_out],
                                              alt_new_to = tiles_wide_remainder)

  rc_coord_data$x[x_cross_in] <- tiles_wide_remainder
  rc_coord_data$y[x_cross_in] <- calc_new_to(foc_from = dup_coord_data$y[x_cross_to],
                                             foc_old_to = dup_coord_data$y[x_cross_in],
                                             alt_from = dup_coord_data$x[x_cross_to],
                                             alt_old_to = dup_coord_data$x[x_cross_in],
                                             alt_new_to = tiles_wide_remainder)
  # remove points which are outside but not adjacent to one inside
  trim_rc_coord_data <- rc_coord_data[!(dup_x_out & !x_cross_out & !x_cross_in), ]

  right_column_coord_data <- data.frame("x" = rep(trim_rc_coord_data$x,
                                                  tiles_high_round) +
                                          mm_xmin +
                                          tiles_wide_round * tile_width,
                                        "y" = rep(trim_rc_coord_data$y,
                                                  tiles_high_round) +
                                          mm_ymin +
                                          rep(tiles_high_seq,
                                              each = nrow(trim_rc_coord_data)) *
                                          tile_height,
                                        "id" = rep(trim_rc_coord_data$id,
                                                   tiles_high_round) +
                                          # then add a displacement based on x tiling
                                          (rep(tiles_high_seq,
                                               each = nrow(trim_rc_coord_data)) *
                                             n_id) +
                                          max(main_coord_data$id))

  full_coord_data <- rbind(main_coord_data,
                           right_column_coord_data)

  # top row

  y_cross_out <- c(FALSE, dup_y_out[-1] & !(dup_y_out[-dup_n_points]) &
                     (dup_coord_data$id[-1] == dup_coord_data$id[-dup_n_points]))
  y_cross_from <- c(y_cross_out[-1], FALSE)

  # find coordinates which are outside but are going inside
  y_cross_in <- c(dup_y_out[-dup_n_points] & !(dup_y_out[-1]) &
                    (dup_coord_data$id[-1] == dup_coord_data$id[-dup_n_points]),
                  FALSE)
  y_cross_to <- c(FALSE, y_cross_in[-dup_n_points])

  tr_coord_data <- dup_coord_data

  tr_coord_data$y[y_cross_out] <- tiles_high_remainder
  tr_coord_data$x[y_cross_out] <- calc_new_to(foc_from = dup_coord_data$x[y_cross_from],
                                              foc_old_to = dup_coord_data$x[y_cross_out],
                                              alt_from = dup_coord_data$y[y_cross_from],
                                              alt_old_to = dup_coord_data$y[y_cross_out],
                                              alt_new_to = tiles_high_remainder)

  tr_coord_data$y[y_cross_in] <- tiles_high_remainder
  tr_coord_data$x[y_cross_in] <- calc_new_to(foc_from = dup_coord_data$x[y_cross_to],
                                             foc_old_to = dup_coord_data$x[y_cross_in],
                                             alt_from = dup_coord_data$y[y_cross_to],
                                             alt_old_to = dup_coord_data$y[y_cross_in],
                                             alt_new_to = tiles_high_remainder)

  # remove points which are outside but not adjacent to one inside
  trim_tr_coord_data <- tr_coord_data[!(dup_y_out & !y_cross_out & !y_cross_in), ]

  top_row_coord_data <- data.frame("x" = rep(trim_tr_coord_data$x,
                                             tiles_wide_round) +
                                     mm_xmin +
                                     rep(tiles_wide_seq,
                                         each = nrow(trim_tr_coord_data)) *
                                     tile_width,
                                   "y" = rep(trim_tr_coord_data$y,
                                             tiles_wide_round) +
                                     mm_ymin +
                                     tiles_high_round * tile_height,
                                   "id" = rep(trim_tr_coord_data$id,
                                              tiles_wide_round) +
                                     (rep(tiles_wide_seq,
                                          each = nrow(trim_tr_coord_data)) *
                                        n_id) +
                                     max(full_coord_data$id))

  full_coord_data <- rbind(full_coord_data,
                           top_row_coord_data)

  # calculate coordinates for the top right corner overlap

  xy_out <- dup_x_out & dup_y_out
  xy_in <- !(dup_x_out | dup_y_out)

  xy_cross_out <- c(FALSE, xy_out[-1] & xy_in[-dup_n_points] &
                      (dup_coord_data$id[-1] == dup_coord_data$id[-dup_n_points]))
  xy_cross_from <- c(xy_cross_out[-1], FALSE)

  # find coordinates which are outside but are going inside
  xy_cross_in <- c(xy_out[-dup_n_points] & xy_in[-1] &
                     (dup_coord_data$id[-1] == dup_coord_data$id[-dup_n_points]),
                   FALSE)
  xy_cross_to <- c(FALSE, xy_cross_in[-dup_n_points])

  trc_coord_data <- dup_coord_data

  # copy in the rows which only cross one boundary
  # remember to trim the easy ones later
  trc_coord_data[x_cross_out | x_cross_in, ] <-
    rc_coord_data[x_cross_out | x_cross_in, ]

  trc_coord_data[y_cross_out | y_cross_in, ] <-
    tr_coord_data[y_cross_out | y_cross_in, ]

  out_xy_dist <- prop_dist(from = dup_coord_data$x[xy_cross_from],
                           old_to = dup_coord_data$x[xy_cross_out],
                           new_to = tiles_wide_remainder) <
    prop_dist(from = dup_coord_data$y[xy_cross_from],
              old_to = dup_coord_data$y[xy_cross_out],
              new_to = tiles_high_remainder)

  # if it crosses x boundary first
  # x is the just the boundary
  # otherwise calculate it
  trc_coord_data$x[xy_cross_out] <-
    ifelse(out_xy_dist,
           tiles_wide_remainder,
           calc_new_to(foc_from = dup_coord_data$x[xy_cross_from],
                       foc_old_to = dup_coord_data$x[xy_cross_out],
                       alt_from = dup_coord_data$y[xy_cross_from],
                       alt_old_to = dup_coord_data$y[xy_cross_out],
                       alt_new_to = tiles_high_remainder)
    )

  # if it crosses the x boundary first
  # calculate y
  # otherwise y is the boundary

  trc_coord_data$y[xy_cross_out] <-
    ifelse(out_xy_dist,
           calc_new_to(foc_from = dup_coord_data$y[xy_cross_from],
                       foc_old_to = dup_coord_data$y[xy_cross_out],
                       alt_from = dup_coord_data$x[xy_cross_from],
                       alt_old_to = dup_coord_data$x[xy_cross_out],
                       alt_new_to = tiles_wide_remainder),
           tiles_high_remainder
    )

  # if it crosses x boundary first
  # x is the just the boundary
  # otherwise calculate it

  in_xy_dist <- prop_dist(from = dup_coord_data$x[xy_cross_to],
                          old_to = dup_coord_data$x[xy_cross_in],
                          new_to = tiles_wide_remainder) <
    prop_dist(from = dup_coord_data$y[xy_cross_to],
              old_to = dup_coord_data$y[xy_cross_in],
              new_to = tiles_high_remainder)

  trc_coord_data$x[xy_cross_in] <-
    ifelse(in_xy_dist,
           tiles_wide_remainder,
           calc_new_to(foc_from = dup_coord_data$x[xy_cross_to],
                       foc_old_to = dup_coord_data$x[xy_cross_in],
                       alt_from = dup_coord_data$y[xy_cross_to],
                       alt_old_to = dup_coord_data$y[xy_cross_in],
                       alt_new_to = tiles_high_remainder)
    )

  # if it crosses the x boundary first
  # calculate y
  # otherwise y is the boundary
  trc_coord_data$y[xy_cross_in] <-
    ifelse(in_xy_dist,
           calc_new_to(foc_from = dup_coord_data$y[xy_cross_to],
                       foc_old_to = dup_coord_data$y[xy_cross_in],
                       alt_from = dup_coord_data$x[xy_cross_to],
                       alt_old_to = dup_coord_data$x[xy_cross_in],
                       alt_new_to = tiles_wide_remainder),
           tiles_high_remainder
    )

  trc_coord_data[x_cross_out | y_cross_out | xy_cross_out, ]
  rep(seq(nrow(trc_coord_data)),
      ifelse(x_cross_out | y_cross_out | xy_cross_out,
             2, 1))

  any_out <- x_cross_out |
          y_cross_out |
          xy_cross_out

  dup_trc_coord_data <- trc_coord_data[rep(seq(nrow(trc_coord_data)),
                                           ifelse(any_out,
                                                  2, 1)), ]

  extra_corners <- (which(any_out) + seq(from = 0,
                                         to = sum(any_out) - 1)) + 1

  dup_trc_coord_data$x[extra_corners] <- tiles_wide_remainder
  dup_trc_coord_data$y[extra_corners] <- tiles_high_remainder

  # remove points which are outside but not adjacent to one inside
  trim_trc_coord_data <- dup_trc_coord_data[!(dup_trc_coord_data$x >
                                            tiles_wide_remainder |
                                              dup_trc_coord_data$y >
                                            tiles_high_remainder), ]

  top_right_corner_coord_data <- trim_trc_coord_data

  top_right_corner_coord_data$x <- trim_trc_coord_data$x + mm_xmin +
    tiles_wide_round * tile_width
  top_right_corner_coord_data$y <- trim_trc_coord_data$y + mm_ymin +
    tiles_high_round * tile_height

  top_right_corner_coord_data$id <- trim_trc_coord_data$id + max(full_coord_data$id)

  # I'm probably leaving in a bug where the line will cut across the top right corner in some circumstances
  # I'd like to split apart the id of any cases where this would be an issue
  # I think it's situations where a line crosses over the x then back over the y
  # or vice versa
  # best just to split the ids of any adjacent outs and backs

  # also with hex I see an overlap in the top right
  # something about my code to figure out which axis is being crossed isn't working
  # might be a perpendicular line issue?

  full_coord_data <- rbind(full_coord_data,
                           top_right_corner_coord_data)

  grid.polyline(x = full_coord_data$x,
                y = full_coord_data$y,
                id = full_coord_data$id,
                default.units = "mm",
                gp = gpar(col = primary_fill,
                          lwd = x$linewidth2))

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


