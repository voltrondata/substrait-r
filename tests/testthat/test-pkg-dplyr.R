
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

test_that("group_by() adds missing groups back in", {
  skip_if_not_installed("dplyr")

  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)

  expect_message(
    compiler %>%
      substrait_group_by(col1) %>%
      select(col2),
    "Adding missing grouping variables: `col1`"
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

test_that("rename_with() for substrait_compiler renames columns using a function", {
  skip_if_not_installed("dplyr")

  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)

  expect_identical(
    dplyr::rename_with(compiler, toupper),
    substrait_project(compiler, "COL1" = col1, "COL2" = col2)
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

test_that("arrange() for substrait_compiler wraps substrait_sort()", {
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

test_that("group_by() for substrait_compiler wraps substrait_group_by()", {
  skip_if_not_installed("dplyr")

  df <- data.frame(
    a = c(1, 1, 2, 2, 3),
    b = c(1, 1, 1, 2, 2),
    c = 1:5
  )

  compiler <- substrait_compiler(df)
  expect_identical(
    dplyr::group_by(compiler, a),
    substrait_group_by(compiler, a)
  )

  grouped <- substrait_group_by(compiler, a)
  expect_identical(
    dplyr::group_by(grouped, b, .add = FALSE),
    substrait_group_by(grouped, b)
  )
  expect_identical(
    dplyr::group_by(grouped, b, .add = TRUE),
    substrait_group_by(grouped, a, b)
  )
})

test_that("ungroup() for substrait_compiler wraps substrait_group_by()", {
  skip_if_not_installed("dplyr")

  df <- data.frame(
    a = c(1, 1, 2, 2, 3),
    b = c(1, 1, 1, 2, 2),
    c = 1:5
  )

  grouped <- substrait_group_by(df, a)
  expect_length(grouped$groups, 1)
  expect_null(dplyr::ungroup(grouped)$groups)
})

test_that("summarise() for substrait_compiler wraps substrait_aggregate()", {
  skip_if_not_installed("dplyr")

  df <- data.frame(
    a = c(1, 1, 2, 2, 3),
    b = c(1, 1, 1, 2, 2),
    c = 1:5
  )

  compiler <- substrait_compiler(df)

  expect_identical(
    dplyr::summarise(compiler, sum(c)),
    substrait_aggregate(compiler, sum(c))
  )

  grouped1 <- substrait_group_by(df, a)
  expect_identical(
    dplyr::summarise(grouped1, sum(c)),
    substrait_aggregate(grouped1, sum(c))
  )
  expect_identical(
    dplyr::summarise(grouped1, sum(c), .groups = "drop"),
    substrait_aggregate(grouped1, sum(c))
  )
  expect_identical(
    dplyr::summarise(grouped1, sum(c), .groups = "keep"),
    substrait_aggregate(grouped1, sum(c)) %>%
      substrait_group_by(a)
  )

  grouped2 <- substrait_group_by(df, a, b)
  expect_identical(
    dplyr::summarise(grouped2, sum(c)),
    substrait_aggregate(grouped2, sum(c)) %>%
      substrait_group_by(a)
  )
  expect_identical(
    dplyr::summarise(grouped2, sum(c), .groups = "drop"),
    substrait_aggregate(grouped2, sum(c))
  )
  expect_identical(
    dplyr::summarise(grouped2, sum(c), .groups = "keep"),
    substrait_aggregate(grouped2, sum(c)) %>%
      substrait_group_by(a, b)
  )
})

test_that("relocate() for substrait_compiler reorders columns", {
  skip_if_not_installed("dplyr")

  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)

  expect_identical(
    dplyr::relocate(compiler, col1, .after = col2),
    substrait_project(compiler, col2, col1)
  )
})

