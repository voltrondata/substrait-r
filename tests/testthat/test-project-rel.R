
test_that("substrait_project() can select all columns unchanged", {
  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)

  result <- substrait_project(compiler, col1, col2)

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

test_that("build_projections can create projection expressions", {
  query <- substrait_dplyr_query(
    mtcars,
    selected_columns = c("carb", "mpg", "disp")
  )

  projections <- build_projections(
    as.data.frame(query),
    attr(query, "selected_columns")
  )

  expect_named(projections[[1]], "selection")
  expect_identical(projections[[1]], simple_integer_field_reference(10L))
  expect_identical(projections[[2]], simple_integer_field_reference(0L))
  expect_identical(projections[[3]], simple_integer_field_reference(2L))
})

test_that("simple_integer_field_reference() returns the correct structure", {
  object <- simple_integer_field_reference(32)
  expect_identical(
    object[["selection"]][["direct_reference"]][["struct_field"]][["field"]],
    32L
  )
})
