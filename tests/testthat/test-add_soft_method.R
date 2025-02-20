test_that("add_soft_method creates an instance of software_method", {
  dt <- dtreg::load_datatype("https://doi.org/21.T11969/b9335ce2c99ed87735a6")
  soft_mehod_inst <- add_soft_method(dt, "stats", "aov")
  expect_equal(dt$software_method()$dt_name, soft_mehod_inst$dt_name)
})

test_that("add_soft_method writes the method label correctly", {
  dt <- dtreg::load_datatype("https://doi.org/21.T11969/b9335ce2c99ed87735a6")
  soft_mehod_inst <- add_soft_method(dt, "stats", "aov")
  expect_equal(soft_mehod_inst$label, "aov")
})

test_that("add_soft_method writes the software library label correctly", {
  dt <- dtreg::load_datatype("https://doi.org/21.T11969/b9335ce2c99ed87735a6")
  soft_mehod_inst <- add_soft_method(dt, "stats", "aov")
  expect_equal(soft_mehod_inst$part_of$label, "stats")
})
