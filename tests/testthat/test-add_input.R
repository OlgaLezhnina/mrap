test_that("add_input throws an error if a wrong argument type", {
  dt <-
    dtreg::load_datatype("https://doi.org/21.T11969/b9335ce2c99ed87735a6")
  expect_error(
    add_input(dt, 11),
    "Argument data_input should be a dataframe, a named list, or a string",
    fixed = TRUE
  )
})

test_that("add_input accepts a string", {
  dt <- dtreg::load_datatype("https://doi.org/21.T11969/b9335ce2c99ed87735a6")
  input_inst <- add_input(dt, "data_url")
  expect_equal(input_inst$source_url, "data_url")
})

test_that("add_input extracts dimensions of a data frame", {
  dt <-
    dtreg::load_datatype("https://doi.org/21.T11969/b9335ce2c99ed87735a6")
  inst <- add_input(dt, iris)
  inst_data_size <-
    list(
      inst$has_characteristic$number_of_rows,
      inst$has_characteristic$number_of_columns
    )
  expected_data_size <- list(nrow(iris), ncol(iris))
  expect_equal(inst_data_size, expected_data_size)
})

test_that("add_input extract names of elements in a named list", {
  dt <-
    dtreg::load_datatype("https://doi.org/21.T11969/b9335ce2c99ed87735a6")
  setosa <- iris[iris$Species == "setosa", ]
  virginica <- iris[iris$Species == "virginica", ]
  input <- list("setosa" = setosa, "virginica" = virginica)
  inst <- add_input(dt, input)
  labels <- list(inst[[1]]$label, inst[[2]]$label)
  expect_equal(labels, list("setosa", "virginica"))
})

test_that("add_input extract dimensions of elements in a named list", {
  dt <-
    dtreg::load_datatype("https://doi.org/21.T11969/b9335ce2c99ed87735a6")
  setosa <- iris[iris$Species == "setosa", ]
  virginica <- iris[iris$Species == "virginica", ]
  input <- list("setosa" = setosa, "virginica" = virginica)
  inst <- add_input(dt, input)
  element_data_size <-
    list(
      inst[[2]]$has_characteristic$number_of_rows,
      inst[[2]]$has_characteristic$number_of_columns
    )
  expected_data_size <- list(nrow(virginica), ncol(virginica))
  expect_equal(element_data_size, expected_data_size)
})

test_that("add_input throws an error if a list is not named", {
  dt <-
    dtreg::load_datatype("https://doi.org/21.T11969/b9335ce2c99ed87735a6")
  setosa <- iris[iris$Species == "setosa", ]
  virginica <- iris[iris$Species == "virginica", ]
  expect_error(
    add_input(dt,
              list(setosa, virginica)),
    "Your input_data list should be named. See 'named list in R'.",
    fixed = TRUE
  )
})
