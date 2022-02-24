library(dplyr)

# make own example data later
#schema <- substrait_schema(mtcars)
schema <- mtcars

test_that("basic selection", {

  out <- base_table(schema) %>%
    select(hp)

  expect_s3_class(out, c("substrait_op", "substrait_select"))
  expect_identical(attributes(out)$cols, list(hp = sym("hp")))

})

test_that("basic selection with multiple variables", {

  out <- base_table(schema) %>%
    select(hp, mpg, am)

  expect_s3_class(out, c("substrait_op", "substrait_select"))
  expect_identical(
    attributes(out)$cols,
    list(
      hp = sym("hp"),
      mpg = sym("mpg"),
      am = sym("am")
    )
  )

})

test_that("select with rename", {

  out <- base_table(schema) %>%
    select(hp2 = hp)

  expect_s3_class(out, c("substrait_op", "substrait_select"))
  expect_identical(attributes(out)$cols, list(hp2 = sym("hp")))

})

test_that("select on select", {

  out <- base_table(schema) %>%
    select(hp, mpg) %>%
    select(hp2 = hp)

  expect_s3_class(out, c("substrait_op", "substrait_select"))
  expect_identical(attributes(out)$cols, list(hp2 = sym("hp")))

})

test_that("can't select columns that don't exist any more", {

  expect_error(
    base_table(schema) %>%
      select(hp, mpg) %>%
      select(carb),
    # Do we want the regex here when it comes from tidyselect??
    regexp = "Column `carb` doesn't exist."
  )

})
