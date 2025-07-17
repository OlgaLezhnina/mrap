test_that("write_analytic_instance adds implemented_by", {
  dt <-
    dtreg::load_datatype("https://doi.org/21.T11969/b9335ce2c99ed87735a6")
  code_string <- "stats::aov(Petal.Length ~ Species, data = iris)"
  inst <- write_analytic_instance(dt, "group_comparison", code_string, iris)

  expect_equal(inst$executes$is_implemented_by, code_string)
})

test_that("write_analytic_instance gets input name from parser", {
  dt <-
    dtreg::load_datatype("https://doi.org/21.T11969/b9335ce2c99ed87735a6")
  code_string <- "stats::aov(Petal.Length ~ Species, data = iris)"
  inst <- write_analytic_instance(dt, "group_comparison", code_string, iris)

  expect_equal(inst$has_input$label, "iris")
})
