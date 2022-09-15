
test_that("substrait_filter() appends a FilterRel to a compiler", {
  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)

  result <- substrait_filter(compiler)

  expect_s3_class(result, "SubstraitCompiler")

  # check that we did append a FilterRel
  expect_identical(
    result$rel$filter$input,
    compiler$rel
  )

  # check that the filter expression is a literal TRUE
  expect_identical(
    result$rel$filter$condition,
    substrait$Expression$create(
      literal = substrait$Expression$Literal$create(boolean = TRUE)
    )
  )

  # check that nothing else about the compiler changed
  expect_identical(result$schema, compiler$schema)
  expect_identical(result$.data, compiler$.data)
})
