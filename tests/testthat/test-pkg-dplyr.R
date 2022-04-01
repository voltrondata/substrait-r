
test_that("dplyr::select() works for substrait_builder", {
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
