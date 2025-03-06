test_that("theme_simple works", {
  vdiffr::expect_doppelganger("theme_simple_plot",
                              ggplot2::ggplot(data = mtcars,
                                              ggplot2::aes(x = cyl, y = mpg)) +
                                ggplot2::geom_point() +
                                theme_simple())
})
