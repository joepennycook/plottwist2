test_that("geom_pattern works", {
  vdiffr::expect_doppelganger("geom_pattern_plot",
                              ggplot2::ggplot(data.frame(factor = c("a", "b", "c"),
                                                         value = c(1, 2, 3)),
                                              ggplot2::aes(x = factor,
                                                           y = value)) +
                                geom_pattern(stat = "identity"))
})
