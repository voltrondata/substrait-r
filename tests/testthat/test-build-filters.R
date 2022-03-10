test_that("build_filters can create filter expressions", {

  query <- substrait_dplyr_query(mtcars, filtered_rows = c(rlang::quo("carb" > 5), rlang::quo("am" == 1)))
  filters <- build_filters(attr(query, "filtered_rows"), as.data.frame(query)[0,])

  expect_named(filters[[1]], "condition")

  outer_function <- filters[[1]][["scalar_function"]]
  expect_equal(outer_function[["function_reference"]], 1)

  filter_1_fields <- outer_function[["args"]][[1]][["scalar_function"]]

  expect_equal(
    filter_1_fields[["function_reference"]],
    2
  )

  expect_selected_field(
    # carb
    filter_1_fields[["args"]][[1]],
    10
  )

  # the 5 from carb > 5
  expect_equal(
    filter_1_fields[["args"]][[2]],
    substrait$Expression$Literal$create(fp64 = 5)
  )

  filter_2_fields <- outer_function[["args"]][[2]][["scalar_function"]]

  expect_equal(
    filter_2_fields[["function_reference"]],
    3
  )

  # am field
  expect_selected_field(
    filter_2_fields[["args"]][[1]],
    8
  )

  # not sure if this should be a float or an int
  expect_equal(
    filter_2_fields[["args"]][[2]],
    substrait$Expression$Literal$create(fp64 = 1)
  )

})
