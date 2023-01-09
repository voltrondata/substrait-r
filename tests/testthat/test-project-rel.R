
test_that("substrait_select() can select all columns unchanged", {
  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)

  result <- substrait_select(compiler, col1, col2)

  expect_s3_class(result, "SubstraitCompiler")

  # check that we did append a ProjectRel
  expect_s3_class(result$rel$project, "substrait_ProjectRel")
  expect_identical(
    result$rel$project$input,
    compiler$rel
  )

  # check that nothing else about the compiler changed
  expect_identical(result$schema, compiler$schema)
  expect_identical(result$.data, compiler$.data)
})

test_that("substrait_project() can add zero columns", {
  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)

  result <- substrait_project(compiler)

  # check that we did append a ProjectRel
  expect_s3_class(result$rel$project, "substrait_ProjectRel")

  # make sure we didn't include an Emit clause
  expect_null(result$rel$project$common$emit)

  # check that nothing else about the compiler changed
  expect_identical(result$schema, compiler$schema)
  expect_identical(result$.data, compiler$.data)
})

test_that("substrait_project() can add columns without an emit clause", {
  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)

  result <- substrait_project(compiler, col3 = 3L)

  # check that we did append a ProjectRel
  expect_s3_class(result$rel$project, "substrait_ProjectRel")

  # make sure we didn't include an unnecessary Emit clause
  expect_null(result$rel$project$common$emit)

  # make sure we actually added a column
  expect_identical(result$schema$names, c("col1", "col2", "col3"))
  expect_identical(names(result$.data), c("col1", "col2", "col3"))
  expect_identical(
    result$schema$struct$types,
    list(
      substrait_fp64(),
      substrait_string(),
      substrait_i32()
    )
  )
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
    projected$.data,
    list(b = simple_integer_field_reference(0))
  )

  expect_identical(projected$schema$names, "b")
  expect_identical(
    projected$schema$struct$types,
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
    projected$.data,
    list(
      b = simple_integer_field_reference(0),
      c = simple_integer_field_reference(1)
    )
  )
})
