
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
  expect_identical(result$.data, compiler$.data)
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
