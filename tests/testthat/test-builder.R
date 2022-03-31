
test_that("substrait_builder() creates a builder with a ReadRel from a data frame", {
  compiler <- substrait_compiler()
  tbl <- data.frame(col1 = 1, col2 = "one")

  builder <- expect_s3_class(
    substrait_builder(tbl, compiler = compiler),
    "substrait_builder"
  )

  expect_identical(builder$names, names(tbl))
  expect_identical(builder$compiler, compiler)
  expect_identical(builder$groups, NULL)
  expect_s3_class(builder$plan, "substrait_Plan")
  expect_match(
    builder$plan$relations[[1]]$rel$read$named_table$names,
    "^named_table_"
  )
})

test_that("substrait_builder() returns its input if it's already a builder", {
  builder <- substrait_builder(data.frame(col1 = 1, col2 = "one"))
  expect_identical(substrait_builder(builder), builder)
})
