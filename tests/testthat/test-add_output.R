test_that("add_generic_output extracts schema name", {
  dt <-
    dtreg::load_datatype("https://doi.org/21.T11969/b9335ce2c99ed87735a6")
  result <- data.frame("A" = 1, "B" = 2)
  inst <- add_generic_output(dt, "group_comparison", result)
  expect_equal(inst$label, "group_comparison results")
})

test_that("add_generic_output extracts data frame", {
  dt <-
    dtreg::load_datatype("https://doi.org/21.T11969/b9335ce2c99ed87735a6")
  result <- data.frame("A" = 1, "B" = 2)
  inst <- add_generic_output(dt, "group_comparison", result)
  expect_equal(inst$source_table$B, 2)
})

test_that("add_evaluation_output extracts named list", {
  dt <-
    dtreg::load_datatype("https://doi.org/21.T11969/5e782e67e70d0b2a022a")
  result <- list("A" = 1, "B" = 2)
  inst <- add_evaluation_output(dt, result)
  expect_equal(inst$source_table$B, 2)
})
