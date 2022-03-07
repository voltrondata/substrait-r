test_that("filter() can subset rows based on a precondition", {
  schema <- mtcars

  out <- base_table(schema) %>%
    dplyr::filter(hp < 100)

  expect_s3_class(out, c("substrait_op", "substrait_filter"))
})
