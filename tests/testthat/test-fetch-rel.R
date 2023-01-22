
test_that("substrait_fetch() appends a FetchRel to a compiler", {
  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)

  result <- substrait_fetch(compiler, 1, 3)

  expect_s3_class(result, "SubstraitCompiler")

  # check that we did append a FetchRel
  expect_identical(
    result$rel$fetch$input,
    compiler$rel
  )

  # check that the fetch offset value is 1 and count value is 3
  expect_identical(result$rel$fetch$offset, 1)
  expect_identical(result$rel$fetch$count, 3)

  # check that nothing else about the compiler changed
  expect_identical(result$schema, compiler$schema)
  expect_identical(result$.data, compiler$.data)
})
