test_that("select() can subset variables by name", {
  schema <- mtcars

  out <- base_table(schema) %>%
    dplyr::select(hp)

  expect_s3_class(out, "substrait_dplyr_query")
  expect_identical(attributes(out)$selected_columns, list(hp = rlang::sym("hp")))
})

test_that("select() can subset with multiple variables", {
  schema <- mtcars

  out <- base_table(schema) %>%
    dplyr::select(hp, mpg, am)

  expect_s3_class(out, "substrait_dplyr_query")
  expect_identical(
    attributes(out)$selected_columns,
    list(
      hp = rlang::sym("hp"),
      mpg = rlang::sym("mpg"),
      am = rlang::sym("am")
    )
  )
})

test_that("select() can rename variables", {
  schema <- mtcars

  out <- base_table(schema) %>%
    dplyr::select(hp2 = hp)

  expect_s3_class(out, "substrait_dplyr_query")
  expect_identical(attributes(out)$selected_columns, list(hp2 = rlang::sym("hp")))
})

test_that("select() can be called multiple times in a chain", {
  schema <- mtcars

  out <- base_table(schema) %>%
    dplyr::select(hp, mpg) %>%
    dplyr::select(hp2 = hp)

  expect_s3_class(out, "substrait_dplyr_query")
  expect_identical(attributes(out)$selected_columns, list(hp2 = rlang::sym("hp")))
})

test_that("select() doesn't work on variables that have been excluded in a previous select()", {
  schema <- mtcars

  expect_snapshot(
    base_table(schema) %>%
      dplyr::select(hp, mpg) %>%
      dplyr::select(carb),
    error = TRUE
  )
})
