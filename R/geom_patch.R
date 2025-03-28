#' Title
#'
#' @param mapping
#' @param data
#' @param stat
#' @param position
#' @param ...
#' @param linejoin
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#'
#' @returns
#' @import ggplot2
#' @import grid
#' @import rlang
#' @export
#'
#' @examples
geom_patch <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       linejoin = "mitre",
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPatch,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}

GeomPatch <- ggproto("GeomPatch", Geom,
                     default_aes = aes(colour = "black",
                                       fill2 = "white",
                                       linewidth = 1.42,
                                       linetype = 1,
                                       alpha = NA,
                                       pattern = 1,
                                       size = 1.5,
                                       fill = "black",
                                       linewidth2 = 1.42),

                     required_aes = c("xmin", "xmax", "ymin", "ymax"),

                     draw_panel = function(self, data,
                                           panel_params, coord,
                                           lineend = "butt",
                                           linejoin = "mitre") {
                       coords <- coord$transform(data, panel_params)

                       grob_list <- lapply(seq(nrow(coords)),
                                           function(i_pattern) {
                         patchGrob(xmin = unit(coords$xmin[i_pattern],
                                               "native"),
                                   xmax = unit(coords$xmax[i_pattern],
                                               "native"),
                                   ymin = unit(coords$ymin[i_pattern],
                                               "native"),
                                   ymax = unit(coords$ymax[i_pattern],
                                               "native"),
                                   pattern = coords$pattern[i_pattern],
                                   size = coords$size[i_pattern],
                                   colour = coords$colour[i_pattern],
                                   linewidth = coords$colour[i_pattern],
                                   fill2 = coords$fill2[i_pattern],
                                   linewidth2 = coords$linewidth2[i_pattern],
                                   fill = coords$fill[i_pattern])
                       })

                       do.call(grobTree, grob_list)
                     },

                     draw_key = draw_key_patch,

                     rename_size = TRUE
)
