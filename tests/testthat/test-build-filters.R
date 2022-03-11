test_that("build_filters can create filter expressions", {

  compiler <- substrait_compiler()

  query <- substrait_dplyr_query(mtcars, filtered_rows = c(rlang::quo(carb > 5), rlang::quo(am == 1)))
  filters <- build_filters(as.data.frame(query), attr(query, "filtered_rows"), compiler)

  expect_length(filters, 2)

  outer_function_1 <- filters[[1]][["scalar_function"]]

  expect_selected_field(
    # carb
    outer_function_1[["args"]][[1]],
    10
  )

  expect_identical(
    compiler$function_extensions_key[["1"]]$name,
    ">"
  )

  # the 5 from carb > 5
  expect_equal(
    outer_function_1[["args"]][[2]],
    substrait$Expression$create(
      substrait$Expression$Literal$create(fp64 = 5)
    )
  )

  outer_function_2 <- filters[[2]][["scalar_function"]]

  # am field
  expect_selected_field(
    outer_function_2[["args"]][[1]],
    8
  )

  expect_identical(
    compiler$function_extensions_key[["2"]]$name,
    "=="
  )

  expect_equal(
    outer_function_2[["args"]][[2]],
    substrait$Expression$create(
      substrait$Expression$Literal$create(fp64 = 1)
    )
  )

})
