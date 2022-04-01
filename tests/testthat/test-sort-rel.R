
test_that("substrait_sort() appends a SortRel to a builder", {
  compiler <- substrait_compiler()
  tbl <- data.frame(col1 = 1, col2 = "one")
  builder <- substrait_builder(tbl)

  result <- substrait_sort(builder)

  expect_s3_class(result, "substrait_builder")

  # check that we did append a SortRel
  expect_identical(
    result$plan$relations[[1]]$rel$sort$input,
    builder$plan$relations[[1]]$rel
  )

  # check that the sorts list is empty
  expect_length(
    result$plan$relations[[1]]$rel$sort$sorts,
    0
  )

  # check that nothing else about the builder changed
  expect_identical(result$schema, builder$schema)
  expect_identical(result$mask, builder$mask)
  expect_identical(result$compiler, builder$compiler)
})

test_that("substrait_sort() expressions can contain substrait_sort_field()", {
  result <- substrait_sort(
    data.frame(a = 1, b = "one"),
    substrait_sort_field(a, "SORT_DIRECTION_DESC_NULLS_LAST")
  )

  expect_identical(
    result$plan$relations[[1]]$rel$sort$sorts[[1]]$direction,
    unclass(substrait$SortField$SortDirection$SORT_DIRECTION_DESC_NULLS_LAST)
  )

  # check with substrait:: prefix too
  result <- substrait_sort(
    data.frame(a = 1, b = "one"),
    substrait::substrait_sort_field(a, "SORT_DIRECTION_DESC_NULLS_LAST")
  )

  expect_identical(
    result$plan$relations[[1]]$rel$sort$sorts[[1]]$direction,
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