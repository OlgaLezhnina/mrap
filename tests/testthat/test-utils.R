test_that("check_argument throws an error if no argument", {
  expect_error(check_argument(),
               "Please provide all required arguments",
               fixed = TRUE)
})

test_that("parse_code_string throws an error if argument is not a string", {
  expect_error(
    parse_code_string(11),
    "Argument code_string should be a string containing package::function",
    fixed = TRUE
  )
})


test_that("parse_code_string throws an error if no :: in the string", {
  expect_error(
    parse_code_string("aov(Petal.Length ~ Species, data = iris)"),
    "Argument code_string should be a string containing package::function",
    fixed = TRUE
  )
})

test_that("parse_code_string extracts package name", {
  result <-
    parse_code_string("stats::aov(Petal.Length ~ Species, data = iris)")
  expect_equal(result$pack, "stats")
})


test_that("parse_code_string extracts data name", {
  result <-
    parse_code_string("stats::aov(Petal.Length ~ Species, data = iris)")
  expect_equal(result$data_name, "iris")
})

test_that("parse_code_string extracts target names for manova", {
  result <-
    parse_code_string("stats::manova(cbind(Petal.Length, Petal.Width) ~ Species, data = iris)")
  expect_equal(result$target_name, list("Petal.Length", "Petal.Width"))
})


test_that("parse_code_string throws an error if many targets without cbind",
          {
            expect_error(
              parse_code_string("stats::manova(Petal.Length, Petal.Width ~ Species, data = iris)"),
              "Something went wrong, contact the developers",
              fixed = TRUE
            )
          })

test_that("parse_code_string extracts target name if the same in vectors", {
  result <-
    parse_code_string("stats::t.test(setosa$Petal.Length, virginica$Petal.Length)")
  expect_equal(result$target_name, "Petal.Length")
})

test_that("parse_code_string sees no target name if different in vectors", {
  result <- parse_code_string("stats::t.test(setosa$Sepal.Length, virginica$Petal.Length)")
  expect_equal(result$target_name, NA)
})

test_that("parse_code_string extracts level name when repetitive", {
  result <-
    parse_code_string("lme4::lmer(math ~ homework + (class_size | schid) + (homework | schid))")
  expect_equal(result$level_name, "schid")
})

test_that("parse_code_string extracts multiple level names", {
  result <-
    parse_code_string("lme4::lmer(math ~ homework + (class_size | schid) + (homework | classid))")
  expect_equal(result$level_name, list("schid", "classid"))
})

test_that("parse_code_string sees no level name when no level", {
  result <-
    parse_code_string("lme4::lmer(math ~ homework + class_size))")
  expect_equal(result$level_name, NA)
})
