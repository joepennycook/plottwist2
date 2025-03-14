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

test_that("symbol selection by name works", {
  vdiffr::expect_doppelganger("geom_symbol_plot_name_selection",
                              ggplot2::ggplot(data = iris,
                                              ggplot2::aes(x = Sepal.Length,
                                                           y = Petal.Length)) +
                                geom_symbol(symbol = "octagon"))
})

test_that("symbol selection by recipe works", {
  vdiffr::expect_doppelganger("geom_symbol_plot_recipe_selection",
                              ggplot2::ggplot(data = iris,
                                              ggplot2::aes(x = Sepal.Length,
                                                           y = Petal.Length)) +
                                geom_symbol(symbol = symbol_recipe(x = c(0, 0, 0, 0.875, 0, 0.875, 0, 0, 0,
                                                                           -0.875, 0, -0.875),
                                                                   y = c(0, 1, 0, 0.510, 0, -0.510, 0, -1, 0,
                                                                           -0.510, 0, 0.510),
                                                                   id = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6))))
})

test_that("symbol selection by recipe works with a manual scale", {
  vdiffr::expect_doppelganger("geom_symbol_plot_recipe_manual_scale",
                              ggplot2::ggplot(data = iris,
                                              ggplot2::aes(x = Sepal.Length,
                                                           y = Petal.Length)) +
                                geom_symbol(ggplot2::aes(symbol = Species)) +
    scale_symbol_manual(values = c(symbol_recipe(x = c(-0.707, -0.707, 0.707, 0.707),
                                                 y = c(0.707, -0.707, 0.707, -0.707)),
                                   symbol_recipe(x = c(-0.875, -0.875, 0, 0, 0.875, 0.875),
                                                 y = c(0.510, -0.510, -1, 1, 0.510, -0.510)),
                                   symbol_recipe(x = c(0.707, 0, -0.707, -1, 1, 0.707, 0,
                                                         -0.707),
                                                 y = c(0.707, 1, 0.707, 0, 0, -0.707, -1,
                                                         -0.707)))))
})
