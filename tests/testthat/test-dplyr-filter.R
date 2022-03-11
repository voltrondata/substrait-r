
test_that("filter() with zero conditions is identical to its input", {
  expect_identical(
    base_table(mtcars),
    dplyr::filter(base_table(mtcars))
  )
})

test_that("filter() can subset rows based on an expression", {
  out <- base_table(mtcars) %>%
    dplyr::filter(hp < 100)

  expect_s3_class(out, "substrait_dplyr_query")

  expect_identical(
    rlang::quo_text(attributes(out)$filtered_rows[[1]]),
    "hp < 100"
  )

  expect_length(attributes(out)$filtered_rows, 1)
})

test_that("filter() can be used multiple times with multiple expressions", {
  out <- base_table(mtcars) %>%
    dplyr::filter(hp < 100) %>%
    dplyr::filter(am == 0)

  expect_s3_class(out, "substrait_dplyr_query")

  expect_length(attributes(out)$filtered_rows, 2)

  expect_identical(
    rlang::quo_text(attributes(out)$filtered_rows[[1]]),
    "hp < 100"
  )

  expect_identical(
    rlang::quo_text(attributes(out)$filtered_rows[[2]]),
    "am == 0"
  )
})
