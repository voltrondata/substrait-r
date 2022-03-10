test_that("build_filters can create filter expressions", {

  query <- substrait_dplyr_query(mtcars, filtered_rows = c(quo("carb > 5"), quo("am == 1")))
  filters <- build_filters(attr(query, "filtered_rows"))

  expect_named(filters[[1]], "selection")
  expect_selected_field(filters[[1]],   10L)
  expect_selected_field(filters[[2]],   NULL)
  expect_selected_field(filters[[3]],   2L)

})
