test_that("filter() can subset rows based on a precondition", {
  schema <- mtcars

  out <- base_table(schema) %>%
    dplyr::filter(hp < 100)

  expect_s3_class(out, c("substrait_dplyr_query"))

  expect_length(attributes(out)$filtered_rows, 1)

})

test_that("filter() can be used multiple times with multiple preconditions", {
  schema <- mtcars

  out <- base_table(schema) %>%
    dplyr::filter(hp < 100) %>%
    dplyr::filter(am == 0)

  expect_s3_class(out, c("substrait_dplyr_query"))
  expect_length(attributes(out)$filtered_rows, 2)
})
