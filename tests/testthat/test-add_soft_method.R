test_that("add_soft_method creates an instance with the correct label", {
  dt <- dtreg::load_datatype("https://doi.org/21.T11969/ff5e3f857788d20dd1aa")
  soft_mehod_inst <- add_soft_method("stats", "aov")
  expect_equal(soft_mehod_inst$label, "stats::aov")
})
