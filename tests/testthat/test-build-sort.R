
test_that("build_sort can create sort expressions", {
  query <- substrait_dplyr_query(
    mtcars,
<<<<<<< HEAD
    arrange_vars = c("carb", "mpg", "disp"),
    arrange_desc = c(FALSE, FALSE, TRUE)
=======
    arrange_vars = c("carb", "mpg", "disp")
>>>>>>> Add dplyr::arrange and sort field
  )

  sort_exprs <- build_sort(
    as.data.frame(query),
<<<<<<< HEAD
    attr(query, "arrange_vars"),
    attr(query, "arrange_desc")
=======
    attr(query, "arrange_vars")
>>>>>>> Add dplyr::arrange and sort field
  )

  expect_length(sort_exprs, 3)
  expect_identical(sort_exprs[[1]][["expr"]], simple_integer_field_reference(10L))
<<<<<<< HEAD
  expect_identical(sort_exprs[[1]][["direction"]], 2L)

  expect_identical(sort_exprs[[2]][["expr"]], simple_integer_field_reference(0L))
  expect_identical(sort_exprs[[2]][["direction"]], 2L)

  expect_identical(sort_exprs[[3]][["expr"]], simple_integer_field_reference(2L))
  expect_identical(sort_exprs[[3]][["direction"]], 4L)
=======
  expect_identical(sort_exprs[[2]][["expr"]], simple_integer_field_reference(0L))
  expect_identical(sort_exprs[[3]][["expr"]], simple_integer_field_reference(2L))
>>>>>>> Add dplyr::arrange and sort field
})
