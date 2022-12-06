test_that("arrow translation for mean() works", {
  skip_if_not(has_arrow_with_substrait())

  expect_equal(
    example_data %>%
      arrow_substrait_compiler() %>%
      substrait_aggregate(x = mean(int, na.rm = TRUE)) %>%
      dplyr::collect(),
    tibble::tibble(x = 4.44444444444444)
  )

  expect_warning(
    example_data %>%
      arrow_substrait_compiler() %>%
      substrait_aggregate(x = mean(int, na.rm = FALSE)) %>%
      dplyr::collect(),
    "Missing value removal from aggregate functions not yet supported, switching to na.rm = TRUE"
  )
})

test_that("arrow translation for sum() works", {
  skip_if_not(has_arrow_with_substrait())

  expect_equal(
    example_data %>%
      arrow_substrait_compiler() %>%
      substrait_aggregate(x = sum(int, na.rm = TRUE)) %>%
      dplyr::collect(),
    tibble::tibble(x = 40)
  )

  expect_warning(
    example_data %>%
      arrow_substrait_compiler() %>%
      substrait_aggregate(x = sum(int, na.rm = FALSE)) %>%
      dplyr::collect(),
    "Missing value removal from aggregate functions not yet supported, switching to na.rm = TRUE"
  )
})

test_that("arrow translation for min() works", {
  skip_if_not(has_arrow_with_substrait())

  expect_equal(
    example_data %>%
      arrow_substrait_compiler() %>%
      substrait_aggregate(x = min(int, na.rm = TRUE)) %>%
      dplyr::collect(),
    tibble::tibble(x = -3212)
  )

  expect_warning(
    example_data %>%
      arrow_substrait_compiler() %>%
      substrait_aggregate(x = min(int, na.rm = FALSE)) %>%
      dplyr::collect(),
    "Missing value removal from aggregate functions not yet supported, switching to na.rm = TRUE"
  )
})

test_that("arrow translation for max() works", {
  skip_if_not(has_arrow_with_substrait())

  expect_equal(
    example_data %>%
      arrow_substrait_compiler() %>%
      substrait_aggregate(x = max(int, na.rm = TRUE)) %>%
      dplyr::collect(),
    tibble::tibble(x = 3212)
  )

  expect_warning(
    example_data %>%
      arrow_substrait_compiler() %>%
      substrait_aggregate(x = max(int, na.rm = FALSE)) %>%
      dplyr::collect(),
    "Missing value removal from aggregate functions not yet supported, switching to na.rm = TRUE"
  )
})

test_that("duckdb translation for n() works", {
  skip_if_not(has_duckdb_with_substrait())

  expect_identical(
    example_data %>%
      duckdb_substrait_compiler() %>%
      dplyr::group_by(lgl) %>%
      substrait_aggregate(n = n()) %>%
      dplyr::collect(),
    tibble::tibble(lgl = c(NA, TRUE, FALSE), n = c(3L, 4L, 3L))
  )
})

test_that("duckdb translation for sum() works", {
  skip_if_not(has_duckdb_with_substrait())

  expect_equal(
    example_data %>%
      duckdb_substrait_compiler() %>%
      substrait_aggregate(x = sum(int, na.rm = TRUE)) %>%
      dplyr::collect(),
    tibble::tibble(x = 40)
  )

  expect_warning(
    example_data %>%
      duckdb_substrait_compiler() %>%
      substrait_aggregate(x = sum(int, na.rm = FALSE)) %>%
      dplyr::collect(),
    "Missing value removal from aggregate functions not supported in DuckDB, switching to na.rm = TRUE"
  )
})

test_that("duckdb translation for mean() works", {
  skip_if_not(has_duckdb_with_substrait())

  expect_equal(
    example_data %>%
      duckdb_substrait_compiler() %>%
      substrait_aggregate(x = mean(int, na.rm = TRUE)) %>%
      dplyr::collect(),
    tibble::tibble(x = 4.444444444)
  )

  expect_warning(
    example_data %>%
      duckdb_substrait_compiler() %>%
      substrait_aggregate(x = mean(int, na.rm = FALSE)) %>%
      dplyr::collect(),
    "Missing value removal from aggregate functions not supported in DuckDB, switching to na.rm = TRUE"
  )
})

test_that("duckdb translation for min() works", {
  skip_if_not(has_duckdb_with_substrait())

  expect_equal(
    example_data %>%
      duckdb_substrait_compiler() %>%
      substrait_aggregate(x = min(int, na.rm = TRUE)) %>%
      dplyr::collect(),
    tibble::tibble(x = -3212)
  )

  expect_warning(
    example_data %>%
      duckdb_substrait_compiler() %>%
      substrait_aggregate(x = min(int, na.rm = FALSE)) %>%
      dplyr::collect(),
    "Missing value removal from aggregate functions not supported in DuckDB, switching to na.rm = TRUE"
  )
})

test_that("duckdb translation for max() works", {
  skip_if_not(has_duckdb_with_substrait())

  expect_equal(
    example_data %>%
      duckdb_substrait_compiler() %>%
      substrait_aggregate(x = max(int, na.rm = TRUE)) %>%
      dplyr::collect(),
    tibble::tibble(x = 3212)
  )

  expect_warning(
    example_data %>%
      duckdb_substrait_compiler() %>%
      substrait_aggregate(x = max(int, na.rm = FALSE)) %>%
      dplyr::collect(),
    "Missing value removal from aggregate functions not supported in DuckDB, switching to na.rm = TRUE"
  )
})
