# make own example data later
#schema <- substrait_schema(mtcars)

test_that("basic selection", {

  schema <- mtcars

  out <- base_table(schema) %>%
    dplyr::select(hp)

  expect_s3_class(out, c("substrait_op", "substrait_select"))
  expect_identical(attributes(out)$cols, list(hp = sym("hp")))

})

test_that("basic selection with multiple variables", {

  schema <- mtcars

  out <- base_table(schema) %>%
    dplyr::select(hp, mpg, am)

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

  schema <- mtcars

  out <- base_table(schema) %>%
    dplyr::select(hp2 = hp)

  expect_s3_class(out, c("substrait_op", "substrait_select"))
  expect_identical(attributes(out)$cols, list(hp2 = sym("hp")))

})

test_that("select on select", {

  schema <- mtcars

  out <- base_table(schema) %>%
    dplyr::select(hp, mpg) %>%
    dplyr::select(hp2 = hp)

  expect_s3_class(out, c("substrait_op", "substrait_select"))
  expect_identical(attributes(out)$cols, list(hp2 = sym("hp")))

})

test_that("can't select columns that don't exist any more", {

  schema <- mtcars

  expect_error(
    base_table(schema) %>%
      dplyr::select(hp, mpg) %>%
      dplyr::select(carb),
    # Do we want the regex here when it comes from tidyselect??
    regexp = "Column `carb` doesn't exist."
  )

})
