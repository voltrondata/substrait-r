
test_that("build_sort can create sort expressions", {
  query <- substrait_dplyr_query(
    mtcars,
    arrange_vars = c("carb", "mpg", "disp"),
    arrange_desc = c(FALSE, FALSE, TRUE)
  )

  sort_exprs <- build_sort(
    as.data.frame(query),
    attr(query, "arrange_vars"),
    attr(query, "arrange_desc")
  )

  expect_length(sort_exprs, 3)
  expect_identical(sort_exprs[[1]][["expr"]], simple_integer_field_reference(10L))

  expect_identical(sort_exprs[[1]][["direction"]], 2L)

  expect_identical(sort_exprs[[2]][["expr"]], simple_integer_field_reference(0L))
  expect_identical(sort_exprs[[2]][["direction"]], 2L)

  expect_identical(sort_exprs[[3]][["expr"]], simple_integer_field_reference(2L))
  expect_identical(sort_exprs[[3]][["direction"]], 4L)

})

