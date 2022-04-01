
test_that("dplyr::select() works for substrait_builder", {
  skip_if_not_installed("dplyr")

  compiler <- substrait_compiler()
  tbl <- data.frame(col1 = 1, col2 = "one")
  builder <- substrait_builder(tbl, compiler = compiler)

  result <- dplyr::select(builder, col1, col2)

  expect_identical(
    dplyr::select(builder, col1, col2),
    substrait_project(builder, col1, col2)
  )

  expect_identical(
    dplyr::select(builder, col1, col2),
    substrait_project(builder, col1, col2)
  )

  expect_identical(
    dplyr::select(builder, everything()),
    substrait_project(builder, col1, col2)
  )

  expect_identical(
    dplyr::select(builder, where(is.numeric)),
    substrait_project(builder, col1)
  )
})

test_that("filter() works for substrait_builder", {
  skip_if_not_installed("dplyr")

  compiler <- substrait_compiler()
  tbl <- data.frame(col1 = 1, col2 = "one")
  builder <- substrait_builder(tbl)

  expect_identical(
    dplyr::filter(builder, col1 > 0),
    substrait_filter(builder, col1 > 0)
  )
})
