
test_that("substrait_builder() creates a builder with one relation", {
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
