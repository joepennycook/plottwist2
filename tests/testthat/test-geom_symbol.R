test_that("geom_symbol works", {
  vdiffr::expect_doppelganger("geom_symbol_plot",
                              ggplot2::ggplot(data = mtcars,
                                              ggplot2::aes(x = cyl, y = mpg)) +
                                geom_symbol())
})

test_that("scale_symbol_manual works", {
  vdiffr::expect_doppelganger("geom_symbol_plot_scale",
                              ggplot2::ggplot(data = iris,
                                              ggplot2::aes(x = Sepal.Length,
                                                           y = Petal.Length)) +
                                geom_symbol(aes(symbol = Species)) +
                                scale_symbol_manual(values = c(3, 1, 8)))
})
