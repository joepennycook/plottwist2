test_that("geom_symbol works", {
  vdiffr::expect_doppelganger("geom_symbol_plot",
                              ggplot2::ggplot(data = mtcars,
                                              ggplot2::aes(x = cyl, y = mpg)) +
                                geom_symbol())
})

test_that("default scale works", {
  vdiffr::expect_doppelganger("geom_symbol_plot_default_scale",
                              ggplot2::ggplot(data = iris,
                                              ggplot2::aes(x = Sepal.Length,
                                                           y = Petal.Length)) +
                                geom_symbol(ggplot2::aes(symbol = Species)))
})

test_that("scale_symbol_manual works", {
  vdiffr::expect_doppelganger("geom_symbol_plot_scale_manual",
                              ggplot2::ggplot(data = iris,
                                              ggplot2::aes(x = Sepal.Length,
                                                           y = Petal.Length)) +
                                geom_symbol(ggplot2::aes(symbol = Species)) +
                                scale_symbol_manual(values = c(26, 29, 31)))
})
