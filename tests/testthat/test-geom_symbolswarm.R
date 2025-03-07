test_that("geom_symbolswarm works", {
  vdiffr::expect_doppelganger("geom_symbolswarm_plot",
                              ggplot2::ggplot(iris,
                                              ggplot2::aes(x = Species,
                                                           y = Petal.Length)) +
                                geom_symbolswarm(stat = "identity"))
})
