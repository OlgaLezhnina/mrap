test_that("select_stat_test_diff gives the correct epic schema", {
  dt <- select_stat_test_diff("epic")
  dt_diff <- dt$statistical_test_of_difference()
  expect_equal(dt_diff$dt_id, "ff5e3f857788d20dd1aa")
})

test_that("select_stat_test_diff gives the correct orkg schema", {
  dt <- select_stat_test_diff("orkg")
  dt_diff <- dt$statistical_test_of_difference()
  expect_equal(dt_diff$dt_id, "R836000")
})

test_that("select_stat_test_diff gives error for an unknown dtr", {
  expect_error(dt <- select_stat_test_diff("wrong_one"),
               "The dtr can be only epic or orkg",
               fixed = TRUE)
})
