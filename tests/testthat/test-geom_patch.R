test_that("geom_patch works", {
  vdiffr::expect_doppelganger("geom_patch_plot",
                              ggplot2::ggplot(data.frame("x" = c(0, 1),
                                                         "y" = c(0, 1)),
                                              ggplot2::aes(x = x,
                                                  y = y,)) +
                                geom_patch(xmin = 0.2, xmax = 0.8,
                                           ymin = 0.2, ymax = 0.8))
})
