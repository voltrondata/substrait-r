
test_that("dplyr::select() for substrait_compiler wraps substrait_project()", {
  skip_if_not_installed("dplyr")

  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)

  result <- dplyr::select(compiler, col1, col2)

  expect_identical(
    dplyr::select(compiler, col1, col2),
    substrait_project(compiler, col1, col2)
  )

  expect_identical(
    dplyr::select(compiler, col1, col2),
    substrait_project(compiler, col1, col2)
  )

  expect_identical(
    dplyr::select(compiler, everything()),
    substrait_project(compiler, col1, col2)
  )

  expect_identical(
    dplyr::select(compiler, where(is.numeric)),
    substrait_project(compiler, col1)
  )
})

test_that("rename() for substrait_compiler renames columns", {
  skip_if_not_installed("dplyr")

  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)

  expect_identical(
    dplyr::rename(compiler, col1_renamed = col1),
    substrait_project(compiler, col1_renamed = col1, col2)
  )
})

test_that("filter() for substrait_compiler wraps substrait_filter()", {
  skip_if_not_installed("dplyr")

  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)

  expect_identical(
    dplyr::filter(compiler, col1 > 0),
    substrait_filter(compiler, col1 > 0)
  )
})

test_that("mutate() for substrait_compiler wraps substrait_project()", {
  skip_if_not_installed("dplyr")

  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)

  expect_identical(
    dplyr::mutate(compiler, col1 > 0),
    substrait_project(compiler, col1, col2, col1 > 0)
  )
})

test_that("transmute() for substrait_compiler wraps substrait_project()", {
  skip_if_not_installed("dplyr")

  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)

  expect_identical(
    dplyr::transmute(compiler, col1 > 0),
    substrait_project(compiler, col1 > 0)
  )
})

test_that("arrange()for substrait_compiler wraps substrait_sort()", {
  skip_if_not_installed("dplyr")

  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)

  expect_identical(
    dplyr::arrange(compiler, col1),
    substrait_sort(compiler, col1)
  )

  expect_identical(
    dplyr::arrange(compiler, desc(col1)),
    substrait_sort(
      compiler,
      substrait_sort_field(col1, "SORT_DIRECTION_DESC_NULLS_LAST")
    )
  )
})
