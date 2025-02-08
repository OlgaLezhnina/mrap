test_that("get_data_name returns data name from call with data=", {
  aov_object <- stats::aov(Petal.Length ~ Species, data = iris)
  data_name <- get_data_name(aov_object)
  expect_equal(data_name, "iris")
})

test_that("get_data_name returns data name from call with data$", {
  aov_object <- stats::aov(iris$Petal.Length ~ iris$Species)
  data_name <- get_data_name(aov_object)
  expect_equal(data_name, "iris")
})

test_that("anova summary from mrap::stats_aov is the same as from stats::aov", {
  aov_object_original <- stats::aov(iris$Petal.Length ~ iris$Species)
  result <- stats_aov(iris$Petal.Length ~ iris$Species)
  aov_object_new <- result$anova
  expect_equal(summary(aov_object_original), summary(aov_object_new))
})
