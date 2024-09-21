test_that("add_soft_method creates an instance with correct properties", {
  soft_mehod_inst <- add_soft_method("Wilcoxon")
  expected = c("label", "uses_software", "is_implemented_by", "has_support_url")
  expect_equal(soft_mehod_inst$prop_names, expected)
})
