
test_that("select() can subset variables by name", {
  out <- base_table(mtcars) %>%
    dplyr::select(hp)

  expect_s3_class(out, "substrait_dplyr_query")
  expect_identical(attributes(out)$selected_columns, list(hp = rlang::sym("hp")))
})

test_that("select(everything()) returns the same object as its input", {
  expect_identical(
    base_table(mtcars),
    dplyr::select(base_table(mtcars), dplyr::everything())
  )
})

test_that("select() can subset with multiple variables", {
  out <- base_table(mtcars) %>%
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
  out <- base_table(mtcars) %>%
    dplyr::select(hp2 = hp)

  expect_s3_class(out, "substrait_dplyr_query")
  expect_identical(attributes(out)$selected_columns, list(hp2 = rlang::sym("hp")))
})

test_that("select() can be called multiple times in a chain", {
  out <- base_table(mtcars) %>%
    dplyr::select(hp, mpg) %>%
    dplyr::select(hp2 = hp)

  expect_s3_class(out, "substrait_dplyr_query")
  expect_identical(attributes(out)$selected_columns, list(hp2 = rlang::sym("hp")))
})

test_that("select() doesn't work on variables that have been excluded in a previous select()", {
  expect_snapshot(
    base_table(mtcars) %>%
      dplyr::select(hp, mpg) %>%
      dplyr::select(carb),
    error = TRUE
  )
})
