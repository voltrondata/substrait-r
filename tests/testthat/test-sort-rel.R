
test_that("substrait_sort() appends a SortRel to a compiler", {
  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)

  result <- substrait_sort(compiler)

  expect_s3_class(result, "SubstraitCompiler")

  # check that we did append a SortRel
  expect_identical(
    result$rel$sort$input,
    compiler$rel
  )

  # check that the sorts list is empty
  expect_length(
    result$rel$sort$sorts,
    0
  )

  # check that nothing else about the compiler changed
  expect_identical(result$schema, compiler$schema)
  expect_identical(result$mask, compiler$mask)
})

test_that("substrait_sort() expressions can contain substrait_sort_field()", {
  result <- substrait_sort(
    data.frame(a = 1, b = "one"),
    substrait_sort_field(a, "SORT_DIRECTION_DESC_NULLS_LAST")
  )

  expect_identical(
    result$rel$sort$sorts[[1]]$direction,
    unclass(substrait$SortField$SortDirection$SORT_DIRECTION_DESC_NULLS_LAST)
  )

  # check with substrait:: prefix too
  result <- substrait_sort(
    data.frame(a = 1, b = "one"),
    substrait::substrait_sort_field(a, "SORT_DIRECTION_DESC_NULLS_LAST")
  )

  expect_identical(
    result$rel$sort$sorts[[1]]$direction,
    unclass(substrait$SortField$SortDirection$SORT_DIRECTION_DESC_NULLS_LAST)
  )
})

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
