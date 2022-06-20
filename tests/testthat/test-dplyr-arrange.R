library(dplyr, warn.conflicts = FALSE)
skip_if_not(has_arrow_with_substrait())


# randomize order of rows in test data
tbl <- slice_sample(example_data, prop = 1L)

test_that("arrange() on integer, double, and character columns", {

  compare_dplyr_binding(
    # skip("dplyr::arrange() doesn't currently work in Arrow via Substrait: https://github.com/voltrondata/substrait-r/issues/68")
    engine = "duckdb",
    .input %>%
      arrange(int, chr) %>%
      collect(),
    example_data
  )
  compare_dplyr_binding(
    # skip("dplyr::arrange() doesn't currently work in Arrow via Substrait: https://github.com/voltrondata/substrait-r/issues/68")
    engine = "duckdb",
    .input %>%
      arrange(int, desc(dbl)) %>%
      collect(),
    example_data
  )

  skip("This fails on duckdb too - https://github.com/voltrondata/substrait-r/issues/122")
  compare_dplyr_binding(
    # skip("dplyr::arrange() doesn't currently work in Arrow via Substrait: https://github.com/voltrondata/substrait-r/issues/68")
    engine = "duckdb",
    .input %>%
      arrange(int, desc(desc(dbl))) %>%
      collect(),
    example_data
  )
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      arrange(int) %>%
      arrange(desc(dbl)) %>%
      collect(),
    example_data
  )
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      arrange(int + dbl, chr) %>%
      collect(),
    example_data
  )
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      mutate(zzz = int + dbl, ) %>%
      arrange(zzz, chr) %>%
      collect(),
    example_data
  )
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      mutate(zzz = int + dbl) %>%
      arrange(int + dbl, chr) %>%
      collect(),
    example_data
  )
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      mutate(int + dbl) %>%
      arrange(int + dbl, chr) %>%
      collect(),
    example_data
  )

  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl) %>%
      arrange(int, dbl) %>%
      collect(),
    example_data
  )

  skip(".by_group not yet implemented: https://github.com/voltrondata/substrait-r/issues/158")
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl) %>%
      arrange(int, dbl, .by_group = TRUE) %>%
      collect(),
    example_data
  )

  skip(".by_group not yet implemented: https://github.com/voltrondata/substrait-r/issues/158")
  compare_dplyr_binding(
    .input %>%
      group_by(lgl, grp2) %>%
      arrange(int, dbl, .by_group = TRUE) %>%
      collect(),
    example_data %>%
      mutate(grp2 = ifelse(is.na(lgl), 1L, as.integer(lgl)))
  )

  skip(".by_group not yet implemented: https://github.com/voltrondata/substrait-r/issues/158")
  compare_dplyr_binding(
    .input %>%
      group_by(lgl) %>%
      arrange(.by_group = TRUE) %>%
      pull(lgl),
    example_data
  )

  compare_dplyr_binding(
    .input %>%
      group_by(lgl) %>%
      arrange() %>%
      collect(),
    example_data
  )

  compare_dplyr_binding(
    .input %>%
      arrange() %>%
      collect(),
    example_data
  )

  test_sort_col <- "chr"
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      arrange(!!sym(test_sort_col)) %>%
      collect(),
    example_data %>%
      select(chr, lgl)
  )
  test_sort_cols <- c("int", "dbl")
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      arrange(!!!syms(test_sort_cols)) %>%
      collect(),
    example_data
  )
})

test_that("arrange() on datetime columns", {

  skip("datetime types not yet supported: https://github.com/voltrondata/substrait-r/issues/124")
  compare_dplyr_binding(
    .input %>%
      arrange(dttm, int) %>%
      collect(),
    tbl
  )
  compare_dplyr_binding(
    .input %>%
      arrange(dttm) %>%
      collect(),
    tbl %>%
      select(dttm, lgl)
  )
})

test_that("arrange() on logical columns", {
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      arrange(lgl, int) %>%
      collect(),
    example_data
  )
})

test_that("arrange() with bad inputs", {

  # test on DuckDB here
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      arrange(1) %>%
      collect(),
    example_data
  )
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      arrange(2 + 2) %>%
      collect(),
    example_data
  )

  expect_error(
    tbl %>%
      duckdb_substrait_compiler() %>%
      arrange(aertidjfgjksertyj),
    "not found",
    fixed = TRUE
  )
  expect_error(
    tbl %>%
      duckdb_substrait_compiler() %>%
      arrange(desc(aertidjfgjksertyj + iaermxiwerksxsdqq)),
    "not found",
    fixed = TRUE
  )

  skip("desc not yet properly implemented: https://github.com/voltrondata/substrait-r/issues/125")
  compare_dplyr_error(
    .input %>%
      arrange(desc(int, chr)) %>%
      collect(),
    example_data
  )

  skip("dplyr::arrange() doesn't currently work in Arrow via Substrait: https://github.com/voltrondata/substrait-r/issues/68")
  expect_error(
    tbl %>%
      arrow_substrait_compiler() %>%
      arrange(1),
    "does not contain any field names",
    fixed = TRUE
  )
  expect_error(
    tbl %>%
      arrow_substrait_compiler() %>%
      arrange(2 + 2),
    "does not contain any field names",
    fixed = TRUE
  )
  expect_error(
    tbl %>%
      arrow_substrait_compiler() %>%
      arrange(aertidjfgjksertyj),
    "not found",
    fixed = TRUE
  )
  expect_error(
    tbl %>%
      arrow_substrait_compiler() %>%
      arrange(desc(aertidjfgjksertyj + iaermxiwerksxsdqq)),
    "not found",
    fixed = TRUE
  )

})
