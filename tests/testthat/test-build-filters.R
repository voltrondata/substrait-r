test_that("build_filters can create filter expressions", {

  query <- substrait_dplyr_query(mtcars, filtered_rows = c(quo("carb > 5"), quo("am == 1")))
  filters <- build_filters(attr(query, "filtered_rows"))


})
