test_that("add_target writes a target", {
  dt <-
    dtreg::load_datatype("https://doi.org/21.T11969/b9335ce2c99ed87735a6")
  target <-
    add_target(dt,
               "stats::t.test(setosa$Petal.Length, virginica$Petal.Length)")
  expect_equal(target$label, "Petal.Length")
})

test_that("add_target writes multiple targets", {
  dt <-
    dtreg::load_datatype("https://doi.org/21.T11969/b9335ce2c99ed87735a6")
  target <-
    add_target(dt,
               "stats::manova(cbind(Petal.Length, Petal.Width) ~ Species, data = iris)")
  expect_equal(target[[2]]$label, "Petal.Width")
})

test_that("add_target gives a warning if unrecognized target name", {
  dt <-
    dtreg::load_datatype("https://doi.org/21.T11969/b9335ce2c99ed87735a6")
  expect_warning(
    add_target(
      dt,
      "stats::t.test(setosa$Sepal.Length, virginica$Petal.Length)"
    ),
    "Target label is not available, you can set it manually"
  )
})
