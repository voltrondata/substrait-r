
test_that("substrait_select() can select all columns unchanged", {
  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)

  result <- substrait_select(compiler, col1, col2)

  expect_s3_class(result, "SubstraitCompiler")

  # check that we did append a ProjectRel
  expect_identical(
    result$rel$project$input,
    compiler$rel
  )

  # check that nothing else about the compiler changed
  expect_identical(result$schema, compiler$schema)
  expect_identical(result$mask, compiler$mask)
})

test_that("simple_integer_field_reference() returns the correct structure", {
  object <- simple_integer_field_reference(32)
  expect_identical(
    object[["selection"]][["direct_reference"]][["struct_field"]][["field"]],
    32L
  )
})

test_that("substrait_select() resets the mask and schema after evaluation", {
  projected <- substrait_select(
    data.frame(a = 1, b = 2L),
    b
  )

  expect_identical(
    projected$mask,
    list(b = simple_integer_field_reference(0))
  )

  expect_identical(projected$schema$names, "b")
  expect_identical(
    projected$schema$struct_$types,
    list(substrait_i32())
  )
})

test_that("substrait_select() evaluates arguments in order", {
  projected <- substrait_select(
    data.frame(a = 1),
    b = a,
    c = b
  )

  expect_identical(
    projected$mask,
    list(
      b = simple_integer_field_reference(0),
      c = simple_integer_field_reference(1)
    )
  )
})
