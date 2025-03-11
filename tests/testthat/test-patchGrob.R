test_that("patchGrob works", {
  vdiffr::expect_doppelganger("patchGrob",
                              patchGrob())
})
