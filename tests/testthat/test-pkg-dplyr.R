
test_that("dplyr::select() for substrait_builder wraps substrait_project()", {
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

test_that("rename() for substrait_builder renames columns", {
  skip_if_not_installed("dplyr")

  compiler <- substrait_compiler()
  tbl <- data.frame(col1 = 1, col2 = "one")
  builder <- substrait_builder(tbl, compiler = compiler)

  expect_identical(
    dplyr::rename(builder, col1_renamed = col1),
    substrait_project(builder, col1_renamed = col1, col2)
  )
})

test_that("filter() for substrait_builder wraps substrait_filter()", {
  skip_if_not_installed("dplyr")

  compiler <- substrait_compiler()
  tbl <- data.frame(col1 = 1, col2 = "one")
  builder <- substrait_builder(tbl)

  expect_identical(
    dplyr::filter(builder, col1 > 0),
    substrait_filter(builder, col1 > 0)
  )
})

test_that("mutate() for substrait_builder wraps substrait_project()", {
  skip_if_not_installed("dplyr")

  compiler <- substrait_compiler()
  tbl <- data.frame(col1 = 1, col2 = "one")
  builder <- substrait_builder(tbl)

  expect_identical(
    dplyr::mutate(builder, col1 > 0),
    substrait_project(builder, col1 > 0)
  )
})

test_that("arrange()for substrait_builder wraps substrait_sort()", {
  skip_if_not_installed("dplyr")

  compiler <- substrait_compiler()
  tbl <- data.frame(col1 = 1, col2 = "one")
  builder <- substrait_builder(tbl)

  expect_identical(
    dplyr::arrange(builder, col1),
    substrait_sort(builder, col1)
  )

  expect_identical(
    dplyr::arrange(builder, desc(col1)),
    substrait_sort(
      builder,
      substrait_sort_field(col1, "SORT_DIRECTION_DESC_NULLS_LAST")
    )
  )
})
