test_that("data_analysis writes the correct schema", {
  code_string <- "stats::aov(Petal.Length ~ Species, data = iris)"
  results <- data.frame("A" = 1, "B" = 2)
  inst_gc <- group_comparison(code_string, iris, results)
  inst_da <- data_analysis(inst_gc)
  expect_equal(inst_da$dt_id, "feeb33ad3e4440682a4d")
})

test_that("descriptive_statistics writes the correct schema", {
  code_string <- "base::function(code)"
  results <- data.frame("A" = 1, "B" = 2)
  inst <- descriptive_statistics(code_string, iris, results)
  expect_equal(inst$dt_id, "5b66cb584b974b186f37")
})

test_that("algorithm_evaluation writes the correct schema", {
  code_string <- "base::function(code)"
  results <- list("A" = 1, "B" = 2)
  inst <- algorithm_evaluation(code_string, iris, results)
  expect_equal(inst$dt_id, "5e782e67e70d0b2a022a")
})

test_that("multilevel_analysis writes the correct schema", {
  code_string <-
    "lme4::lmer(math ~ homework + (1 + homework | schid), data=mlmdata)"
  results <- data.frame("A" = 1, "B" = 2)
  inst <- multilevel_analysis(code_string, "data_url", results)
  expect_equal(inst$dt_id, "c6b413ba96ba477b5dca")
})

test_that("correlation_analysis writes the correct schema", {
  code_string <- "base::function(code)"
  results <- data.frame("A" = 1, "B" = 2)
  inst <- correlation_analysis(code_string, iris, results)
  expect_equal(inst$dt_id, "3f64a93eef69d721518f")
})

test_that("group_comparison writes the correct schema", {
  code_string <- "stats::aov(Petal.Length ~ Species, data = iris)"
  results <- data.frame("A" = 1, "B" = 2)
  inst <- group_comparison(code_string, iris, results)
  expect_equal(inst$dt_id, "b9335ce2c99ed87735a6")
})

test_that("regression_analysis writes the correct schema", {
  code_string <- "base::function(code)"
  results <- data.frame("A" = 1, "B" = 2)
  inst <- regression_analysis(code_string, iris, results)
  expect_equal(inst$dt_id, "286991b26f02d58ee490")
})

test_that("class_prediction writes the correct schema", {
  code_string <- "base::function(code)"
  results <- data.frame("A" = 1, "B" = 2)
  inst <- class_prediction(code_string, iris, results)
  expect_equal(inst$dt_id, "6e3e29ce3ba5a0b9abfe")
})

test_that("class_discovery writes the correct schema", {
  code_string <- "base::function(code)"
  results <- data.frame("A" = 1, "B" = 2)
  inst <- class_discovery(code_string, iris, results)
  expect_equal(inst$dt_id, "c6e19df3b52ab8d855a9")
})

test_that("factor_analysis writes the correct schema", {
  code_string <- "base::function(code)"
  results <- data.frame("A" = 1, "B" = 2)
  inst <- factor_analysis(code_string, iris, results)
  expect_equal(inst$dt_id, "437807f8d1a81b5138a3")
})
