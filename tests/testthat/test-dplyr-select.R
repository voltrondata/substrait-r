test_that("basic selection", {
  x <- unbound_table(mtcars, "tableymctableface")
  out <- select(x, hp, mpg)

  expect_named(out, "project")
  expect_length(out$project$expressions, 2)
  expect_equal(out$project$expressions[[1]]$selection$direct_reference$struct_field$field, 4)
  expect_equal(out$project$expressions[[2]]$selection$direct_reference$struct_field$field, 1)

})
