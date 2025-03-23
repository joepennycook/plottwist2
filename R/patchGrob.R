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

  tiles_wide_seq <- seq(tiles_wide_round) - 1
  tiles_high_seq <- seq(tiles_high_round) - 1

  # extrapolate the tile coordinates to the main body of the patch
  main_coord_data <- data.frame("x" = rep(coord_data$x,
                                          tiles_high_round * tiles_wide_round) +
                                  mm_xmin +
                                  rep(rep(tiles_wide_seq,
                                          each = nrow(coord_data)),
                                      times = tiles_high_round) * tile_width,
                                "y" = rep(coord_data$y, tiles_high_round *
                                            tiles_wide_round) +
                                  mm_ymin +
                                  rep(tiles_high_seq,
                                      each = nrow(coord_data) *
                                        tiles_wide_round) *
                                  tile_height,
                                "id" = rep(coord_data$id,
                                           tiles_high_round *
                                             tiles_wide_round) +
                                  # then add a displacement based on x tiling
                                  (rep(rep(tiles_wide_seq,
                                           each = nrow(coord_data)),
                                       times = tiles_high_round) * n_id) +
                                  # then add a displacement based on y tiling
                                  (rep(tiles_high_seq,
                                       each = nrow(coord_data) *
                                         tiles_wide_round) *
                                     tiles_wide_round * n_id))

  # draw most of the pattern
  grid.polyline(x = main_coord_data$x,
                y = main_coord_data$y,
                id = main_coord_data$id,
                default.units = "mm",
                gp = gpar(col = primary_fill,
                          lwd = x$linewidth2))

  # calculate coordinates for the right column overlap

  rc_coord_data <- data.frame()

  for (i in 1:nrow(coord_data)) {
    # repeat for all coordinates in pattern tile
    if(coord_data$x[i] <= tiles_wide_remainder) {
      # if the coordinates are inside the patch, keep them
      rc_coord_data <- rbind(rc_coord_data, coord_data[i, ])
    } else {
      # but if the coordinates are outside of the patch...
      if (i != 1) {
        # so long as we're not looking at the first coordinate...
        if ((coord_data$x[i - 1] <= tiles_wide_remainder) &
            (coord_data$id[i] == coord_data$id[i - 1])) {
          # if the previous coordinates are within the patch
          # and part of the same line
          # calculate the intersection of the line with the border
          # and save the intersection as a new coordinate point

          xs <- coord_data$x[(i - 1):i]
          ys <- coord_data$y[(i - 1):i]

          new_y <- predict(lm(ys ~ xs), data.frame("xs" = tiles_wide_remainder))

          rc_coord_data <- rbind(rc_coord_data,
                                 data.frame("x" = tiles_wide_remainder,
                                            "y" = new_y,
                                            "id" = coord_data$id[i]))
        }
      }

      if (i != nrow(coord_data)) {
        # so long as we're not looking at the last coordinate...
        if ((coord_data$x[i + 1] <= tiles_wide_remainder) &
            (coord_data$id[i] == coord_data$id[i + 1])) {
          # if the next coordinates are in the patch
          # and part of the same line
          # calculate the intersection and save it

          xs <- coord_data$x[i:(i + 1)]
          ys <- coord_data$y[i:(i + 1)]

          new_y <- predict(lm(ys ~ xs), data.frame("xs" = tiles_wide_remainder))

          rc_coord_data <- rbind(rc_coord_data,
                                 data.frame("x" = tiles_wide_remainder,
                                            "y" = new_y,
                                            "id" = coord_data$id[i]))
        }
      }
    }
  }

  # draw the right column of tiles

  if (nrow(rc_coord_data) > 0) {
    for (j in 1:tiles_high_round) {
      grid.polyline(x = rc_coord_data$x + mm_xmin +
                      (tiles_wide_round * tile_width),
                    y = rc_coord_data$y + mm_ymin + ((j - 1) * tile_height),
                    id = rc_coord_data$id,
                    default.units = "mm",
                    gp = gpar(col = primary_fill,
                              lwd = x$linewidth2))
    }
  }

  # calculate coordinates for the top row overlap

  tr_coord_data <- data.frame()

  for (i in 1:nrow(coord_data)) {
    # repeat for all coordinates in pattern tile
    if(coord_data$y[i] <= tiles_high_remainder) {
      # if the coordinates are inside the patch, keep them
      tr_coord_data <- rbind(tr_coord_data, coord_data[i, ])
    } else {
      # but if the coordinates are outside of the patch...
      if (i != 1) {
        # so long as we're not looking at the first coordinate...
        if ((coord_data$y[i - 1] <= tiles_high_remainder) &
            (coord_data$id[i] == coord_data$id[i - 1])) {
          # if the previous coordinates are within the patch
          # and part of the same line
          # calculate the intersection of the line with the border
          # and save the intersection as a new coordinate point

          xs <- coord_data$x[(i - 1):i]
          ys <- coord_data$y[(i - 1):i]

          new_x <- predict(lm(xs ~ ys), data.frame("ys" = tiles_high_remainder))

          tr_coord_data <- rbind(tr_coord_data,
                                 data.frame("x" = new_x,
                                            "y" = tiles_high_remainder,
                                            "id" = coord_data$id[i]))
        }
      }

      if (i != nrow(coord_data)) {
        # so long as we're not looking at the last coordinate...
        if ((coord_data$y[i + 1] <= tiles_high_remainder) &
            (coord_data$id[i] == coord_data$id[i + 1])) {
          # if the next coordinates are in the patch
          # and part of the same line
          # calculate the intersection and save it

          xs <- coord_data$x[i:(i + 1)]
          ys <- coord_data$y[i:(i + 1)]

          new_x <- predict(lm(xs ~ ys), data.frame("ys" = tiles_high_remainder))

          tr_coord_data <- rbind(tr_coord_data,
                                 data.frame("x" = new_x,
                                            "y" = tiles_high_remainder,
                                            "id" = coord_data$id[i]))
        }
      }
    }
  }

  # draw the top row of tiles

  if (nrow(tr_coord_data) > 0) {
    for (i in 1:tiles_wide_round) {
      grid.polyline(x = tr_coord_data$x + mm_xmin + ((i - 1) * tile_width),
                    y = tr_coord_data$y + mm_ymin +
                      (tiles_high_round * tile_height),
                    id = tr_coord_data$id,
                    default.units = "mm",
                    gp = gpar(col = primary_fill,
                              lwd = x$linewidth2))
    }
  }

  # calculate coordinates for the top right corner overlap

  trc_coord_data <- data.frame()

  for (i in 1:nrow(coord_data)) {
    # repeat for all coordinates in pattern tile
    if(coord_data$y[i] <= tiles_high_remainder &
       coord_data$x[i] <= tiles_wide_remainder) {
      # if the coordinates are inside the patch, keep them
      trc_coord_data <- rbind(trc_coord_data, coord_data[i, ])
    } else {
      # but if the coordinates are outside of the patch...
      if (i != 1) {
        # so long as we're not looking at the first coordinate...
        if ((coord_data$y[i - 1] <= tiles_high_remainder &
             coord_data$x[i - 1] <= tiles_wide_remainder) &
            (coord_data$id[i] == coord_data$id[i - 1])) {
          # if the previous coordinates are within the patch
          # and part of the same line
          # calculate the intersection of the line with the border
          # and save the intersection as a new coordinate point

          xs <- coord_data$x[(i - 1):i]
          ys <- coord_data$y[(i - 1):i]

          if (xs[1] == xs[2]) {
            # if the line is parallel to the y axis
            trc_coord_data <- rbind(trc_coord_data,
                                    data.frame("x" = c(xs[1],
                                                       tiles_wide_remainder),
                                               "y" = rep(tiles_high_remainder,
                                                         2),
                                               "id" = rep(coord_data$id[i], 2)))
          } else if (ys[1] == ys[2]) {
            # or if the line is parallel to the x axis
            trc_coord_data <- rbind(trc_coord_data,
                                    data.frame("x" = rep(tiles_wide_remainder,
                                                         2),
                                               "y" = c(ys[1],
                                                       tiles_high_remainder),
                                               "id" = rep(coord_data$id[i], 2)))
          } else {
            # otherwise figure out which line is shorter
            new_x <- predict(lm(xs ~ ys),
                             data.frame("ys" = tiles_high_remainder))
            new_y <- predict(lm(ys ~ xs),
                             data.frame("xs" = tiles_wide_remainder))

            dist_high <- (((xs[1] - new_x) ^ 2) +
                            ((ys[1] - tiles_high_remainder) ^ 2)) ^ 0.5
            dist_wide <- (((xs[1] - tiles_wide_remainder) ^ 2) +
                            ((ys[1] - new_y) ^ 2)) ^ 0.5

            if (dist_high <= dist_wide) {
              trc_coord_data <- rbind(trc_coord_data,
                                      data.frame("x" = c(new_x,
                                                         tiles_wide_remainder),
                                                 "y" = rep(tiles_high_remainder,
                                                           2),
                                                 "id" = rep(coord_data$id[i],
                                                            2)))
            } else {
              trc_coord_data <- rbind(trc_coord_data,
                                      data.frame("x" = rep(tiles_wide_remainder,
                                                           2),
                                                 "y" = c(new_y,
                                                         tiles_high_remainder),
                                                 "id" = rep(coord_data$id[i],
                                                            2)))
            }
          }
        }
      }

      if (i != nrow(coord_data)) {
        # so long as we're not looking at the last coordinate...
        if ((coord_data$y[i + 1] <= tiles_high_remainder &
             coord_data$x[i + 1] <= tiles_wide_remainder) &
            (coord_data$id[i] == coord_data$id[i + 1])) {
          # if the next coordinates are in the patch
          # and part of the same line
          # calculate the intersection and save it

          xs <- coord_data$x[i:(i + 1)]
          ys <- coord_data$y[i:(i + 1)]

          if (xs[1] == xs[2]) {
            # if the line is parallel to the y axis
            trc_coord_data <- rbind(trc_coord_data,
                                    data.frame("x" = xs[1],
                                               "y" = tiles_high_remainder,
                                               "id" = coord_data$id[i]))
          } else if (ys[1] == ys[2]) {
            # or if the line is parallel to the x axis
            trc_coord_data <- rbind(trc_coord_data,
                                    data.frame("x" = tiles_wide_remainder,
                                               "y" = ys[1],
                                               "id" = coord_data$id[i]))
          } else {
            new_x <- predict(lm(xs ~ ys),
                             data.frame("ys" = tiles_high_remainder))
            new_y <- predict(lm(ys ~ xs),
                             data.frame("xs" = tiles_wide_remainder))

            dist_high <- (((xs[2] - new_x) ^ 2) +
                            ((ys[2] - tiles_high_remainder) ^ 2)) ^ 0.5
            dist_wide <- (((xs[2] - tiles_wide_remainder) ^ 2) +
                            ((ys[2] - new_y) ^ 2)) ^ 0.5

            if (dist_high <= dist_wide) {
              trc_coord_data <- rbind(trc_coord_data,
                                      data.frame("x" = new_x,
                                                 "y" = tiles_high_remainder,
                                                 "id" = coord_data$id[i]))
            } else {
              trc_coord_data <- rbind(trc_coord_data,
                                      data.frame("x" = tiles_wide_remainder,
                                                 "y" = new_y,
                                                 "id" = coord_data$id[i]))
            }
          }
        }
      }
    }
  }

  # draw the final partial tile

  if (nrow(trc_coord_data) > 0) {
    grid.polyline(x = trc_coord_data$x + mm_xmin +
                    (tiles_wide_round * tile_width),
                  y = trc_coord_data$y + mm_ymin +
                    (tiles_high_round * tile_height),
                  id = trc_coord_data$id,
                  default.units = "mm",
                  gp = gpar(col = primary_fill,
                            lwd = x$linewidth2))
  }

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
