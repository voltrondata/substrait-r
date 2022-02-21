library(dplyr)

test_that("basic selection", {
  x <- base_table(mtcars)
  out <- select(x, cyl, carb)

  expect_named(out, "project")
  expect_length(out$project$expressions, 2)
  expect_equal(out$project$expressions[[1]]$selection$direct_reference$struct_field$field, 1)
  expect_equal(out$project$expressions[[2]]$selection$direct_reference$struct_field$field, 10)

})

# test_that("two selects equivalent to one", {
#   x <- base_table(mtcars)
#   out <- select(x, cyl, carb) %>%
#     select(cyl)
#
#   expect_named(out, "project")
#   expect_length(out$project$expressions, 2)
#   expect_equal(out$project$expressions[[1]]$selection$direct_reference$struct_field$field, 1)
#   expect_equal(out$project$expressions[[2]]$selection$direct_reference$struct_field$field, 10)
#
# })
