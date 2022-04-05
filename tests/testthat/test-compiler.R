
test_that("substrait_compiler() creates a builder with a ReadRel from a data frame", {
  tbl <- data.frame(col1 = 1, col2 = "one")

  expect_s3_class(
    compiler <- substrait_compiler(tbl),
    "SubstraitCompiler"
  )

  expect_identical(
    compiler$schema,
    substrait$NamedStruct$create(
      names = c("col1", "col2"),
      struct_ = substrait$Type$Struct$create(
        types = list(
          substrait_fp64(),
          substrait_string()
        )
      )
    )
  )

  expect_identical(
    compiler$mask,
    list(
      col1 = simple_integer_field_reference(0L),
      col2 = simple_integer_field_reference(1L)
    )
  )

  expect_identical(compiler$groups, NULL)
  expect_s3_class(compiler$rel, "substrait_Rel")
  expect_identical(
    compiler$named_table(compiler$rel$read$named_table$names),
    tbl
  )
})

test_that("substrait_compiler() returns its input if it's already a compiler", {
  compiler <- substrait_compiler(data.frame(col1 = 1, col2 = "one"))
  expect_identical(substrait_compiler(compiler), compiler)
})
