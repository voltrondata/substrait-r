test_that("build_projections can create projection expressions", {

  query <- substrait_dplyr_query(mtcars, selected_columns = c("carb", "mpg", "disp"))
  projections <- build_projections(as.data.frame(query), attr(query, "selected_columns"))

  expect_named(projections[[1]], "selection")
  expect_selected_field(projections[[1]], 10L)
  expect_selected_field(projections[[2]], NULL)
  expect_selected_field(projections[[3]], 2L)

})
