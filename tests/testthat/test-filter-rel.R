
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
  expect_identical(result$mask, compiler$mask)
})

test_that("build_filters can create filter expressions", {
  compiler <- SubstraitCompiler$new()

  query <- substrait_dplyr_query(
    mtcars,
    filtered_rows = list(rlang::quo(carb > 5), rlang::quo(am == 1))
  )
  filters <- build_filters(
    as.data.frame(query),
    attr(query, "filtered_rows"),
    compiler
  )

  expect_length(filters[[1]][["args"]], 2)

  outer_function_1 <- filters[[1]][["args"]][[1]][["scalar_function"]]

  expect_identical(
    # carb
    outer_function_1[["args"]][[1]],
    simple_integer_field_reference(10L)
  )

  expect_identical(
    compiler$function_extension(1)$name,
    ">"
  )

  # the 5 from carb > 5
  expect_equal(
    outer_function_1[["args"]][[2]],
    substrait$Expression$create(
      literal = substrait$Expression$Literal$create(fp64 = 5)
    )
  )

  outer_function_2 <- filters[[1]][["args"]][[2]][["scalar_function"]]

  # am field
  expect_identical(
    outer_function_2[["args"]][[1]],
    simple_integer_field_reference(8)
  )

  expect_identical(
    compiler$function_extension(2)$name,
    "=="
  )

  expect_equal(
    outer_function_2[["args"]][[2]],
    substrait$Expression$create(
      literal = substrait$Expression$Literal$create(fp64 = 1)
    )
  )
})
