test_that("add_level writes a level", {
  dt <-
    dtreg::load_datatype("https://doi.org/21.T11969/b9335ce2c99ed87735a6")
  level <-
    add_level(dt,
              "lme4::lmer(math ~ homework + (class_size | schid) + (homework | schid))")
  expect_equal(level$label, "schid")
})

test_that("add_level writes multiple levels", {
  dt <-
    dtreg::load_datatype("https://doi.org/21.T11969/b9335ce2c99ed87735a6")
  level <-
    add_level(dt,
              "lme4::lmer(math ~ homework + (class_size | schid) + (homework | classid))")
  expect_equal(level[[2]]$label, "classid")
})

test_that("add_level gives a warning if unrecognized level name", {
  dt <-
    dtreg::load_datatype("https://doi.org/21.T11969/b9335ce2c99ed87735a6")
  expect_warning(
    add_level(dt,
              "lme4::lmer(math ~ homework + class_size))"),
    "Level label is not available, you can set it manually"
  )
})
