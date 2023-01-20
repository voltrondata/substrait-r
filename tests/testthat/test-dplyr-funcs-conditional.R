test_that("arrow translation for != works", {
  skip_if_not(has_arrow_with_substrait())

  expect_equal(
    example_data[1:5, "dbl"] %>%
      arrow_substrait_compiler() %>%
      substrait_filter(dbl != 0) %>%
      dplyr::collect(),
    tibble::tibble(dbl = c(-999, -99, -9, 9))
  )
})

test_that("arrow translation for == works", {
  skip_if_not(has_arrow_with_substrait())

  expect_equal(
    example_data[1:5, "dbl"] %>%
      arrow_substrait_compiler() %>%
      substrait_filter(dbl == 0) %>%
      dplyr::collect(),
    tibble::tibble(dbl = 0)
  )
})

test_that("arrow translation for < works", {
  skip_if_not(has_arrow_with_substrait())

  expect_equal(
    example_data[1:5, "dbl"] %>%
      arrow_substrait_compiler() %>%
      substrait_filter(dbl < 0) %>%
      dplyr::collect(),
    tibble::tibble(dbl = c(-999, -99, -9))
  )
})

test_that("arrow translation for > works", {
  skip_if_not(has_arrow_with_substrait())

  expect_equal(
    example_data[1:5, "dbl"] %>%
      arrow_substrait_compiler() %>%
      substrait_filter(dbl > 0) %>%
      dplyr::collect(),
    tibble::tibble(dbl = 9)
  )
})

test_that("arrow translation for <= works", {
  skip_if_not(has_arrow_with_substrait())

  expect_equal(
    example_data[1:5, "dbl"] %>%
      arrow_substrait_compiler() %>%
      substrait_filter(dbl <= 0) %>%
      dplyr::collect(),
    tibble::tibble(dbl = c(-999, -99, -9, 0))
  )
})

test_that("arrow translation for >= works", {
  skip_if_not(has_arrow_with_substrait())

  expect_equal(
    example_data[1:5, "dbl"] %>%
      arrow_substrait_compiler() %>%
      substrait_filter(dbl >= 0) %>%
      dplyr::collect(),
    tibble::tibble(dbl = c(0, 9))
  )
})

test_that("duckdb translation for != works", {
  skip_if_not(has_duckdb_with_substrait())

  expect_equal(
    example_data[1:5, "dbl"] %>%
      duckdb_substrait_compiler() %>%
      substrait_filter(dbl != 0) %>%
      dplyr::collect(),
    tibble::tibble(dbl = c(-999, -99, -9, 9))
  )
})

test_that("duckdb translation for == works", {
  skip_if_not(has_duckdb_with_substrait())

  expect_equal(
    example_data[1:5, "dbl"] %>%
      duckdb_substrait_compiler() %>%
      substrait_filter(dbl == 0) %>%
      dplyr::collect(),
    tibble::tibble(dbl = 0)
  )
})

test_that("duckdb translation for < works", {
  skip_if_not(has_duckdb_with_substrait())

  expect_equal(
    example_data[1:5, "dbl"] %>%
      duckdb_substrait_compiler() %>%
      substrait_filter(dbl < 0) %>%
      dplyr::collect(),
    tibble::tibble(dbl = c(-999, -99, -9))
  )
})

test_that("duckdb translation for > works", {
  skip_if_not(has_duckdb_with_substrait())

  expect_equal(
    example_data[1:5, "dbl"] %>%
      duckdb_substrait_compiler() %>%
      substrait_filter(dbl > 0) %>%
      dplyr::collect(),
    tibble::tibble(dbl = 9)
  )
})

test_that("duckdb translation for <= works", {
  skip_if_not(has_duckdb_with_substrait())

  expect_equal(
    example_data[1:5, "dbl"] %>%
      duckdb_substrait_compiler() %>%
      substrait_filter(dbl <= 0) %>%
      dplyr::collect(),
    tibble::tibble(dbl = c(-999, -99, -9, 0))
  )
})

test_that("duckdb translation for >= works", {
  skip_if_not(has_duckdb_with_substrait())

  expect_equal(
    example_data[1:5, "dbl"] %>%
      duckdb_substrait_compiler() %>%
      substrait_filter(dbl >= 0) %>%
      dplyr::collect(),
    tibble::tibble(dbl = c(0, 9))
  )
})
