test_that("add_software_method creates an instance of software_method", {
  dt <- dtreg::load_datatype("https://doi.org/21.T11969/b9335ce2c99ed87735a6")
  sw_method_inst <- add_software_method(dt, "stats::aov(Petal.Length ~ Species, data = iris)")
  expect_equal(dt$software_method()$dt_name, sw_method_inst$dt_name)
})

test_that("add_software_method writes the method label correctly", {
  dt <- dtreg::load_datatype("https://doi.org/21.T11969/b9335ce2c99ed87735a6")
  sw_method_inst <- add_software_method(dt, "stats::aov(Petal.Length ~ Species, data = iris)")
  expect_equal(sw_method_inst$label, "aov")
})

test_that("add_software_method writes the software library label correctly", {
  dt <- dtreg::load_datatype("https://doi.org/21.T11969/b9335ce2c99ed87735a6")
  sw_method_inst <- add_software_method(dt, "stats::aov(Petal.Length ~ Species, data = iris)")
  expect_equal(sw_method_inst$part_of$label, "stats")
})
