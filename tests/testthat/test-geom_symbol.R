test_that("geom_symbol works", {
  vdiffr::expect_doppelganger("geom_symbol_plot",
                              ggplot2::ggplot(data = mtcars,
                                              ggplot2::aes(x = cyl, y = mpg)) +
                                geom_symbol())
})
