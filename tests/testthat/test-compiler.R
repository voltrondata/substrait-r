
test_that("substrait_compiler() creates a builder with a ReadRel from a data frame", {
  consumer <- SubstraitCompiler$new()
  tbl <- data.frame(col1 = 1, col2 = "one")

  expect_s3_class(
    builder <- substrait_compiler(tbl, consumer = consumer),
    "substrait_compiler"
  )

  expect_identical(
    builder$schema,
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
    builder$mask,
    list(
      col1 = simple_integer_field_reference(0L),
      col2 = simple_integer_field_reference(1L)
    )
  )

  expect_identical(builder$consumer, consumer)
  expect_identical(builder$groups, NULL)
  expect_s3_class(builder$rel, "substrait_Rel")
  expect_identical(
    consumer$named_table(builder$rel$read$named_table$names),
    tbl
  )
})

test_that("substrait_compiler() returns its input if it's already a builder", {
  builder <- substrait_compiler(data.frame(col1 = 1, col2 = "one"))
  expect_identical(substrait_compiler(builder), builder)
})
