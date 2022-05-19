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
    tbl
  )
  compare_dplyr_binding(
    # skip("dplyr::arrange() doesn't currently work in Arrow via Substrait: https://github.com/voltrondata/substrait-r/issues/68")
    engine = "duckdb",
    .input %>%
      arrange(int, desc(dbl)) %>%
      collect(),
    tbl
  )

  skip("This fails on duckdb too - https://github.com/voltrondata/substrait-r/issues/122")
  compare_dplyr_binding(
    # skip("dplyr::arrange() doesn't currently work in Arrow via Substrait: https://github.com/voltrondata/substrait-r/issues/68")
    engine = "duckdb",
    .input %>%
      arrange(int, desc(desc(dbl))) %>%
      collect(),
    tbl
  )
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      arrange(int) %>%
      arrange(desc(dbl)) %>%
      collect(),
    tbl
  )
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      arrange(int + dbl, chr) %>%
      collect(),
    tbl
  )
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      mutate(zzz = int + dbl, ) %>%
      arrange(zzz, chr) %>%
      collect(),
    tbl
  )
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      mutate(zzz = int + dbl) %>%
      arrange(int + dbl, chr) %>%
      collect(),
    tbl
  )
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      mutate(int + dbl) %>%
      arrange(int + dbl, chr) %>%
      collect(),
    tbl
  )

  skip("group_by not yet implemented: https://github.com/voltrondata/substrait-r/issues/28")
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl) %>%
      arrange(int, dbl) %>%
      collect(),
    tbl
  )

  skip("group_by not yet implemented: https://github.com/voltrondata/substrait-r/issues/28")
  compare_dplyr_binding(
    .input %>%
      group_by(lgl) %>%
      arrange(int, dbl, .by_group = TRUE) %>%
      collect(),
    tbl
  )

  skip("group_by not yet implemented: https://github.com/voltrondata/substrait-r/issues/28")
  compare_dplyr_binding(
    .input %>%
      group_by(lgl, grp2) %>%
      arrange(int, dbl, .by_group = TRUE) %>%
      collect(),
    tbl %>%
      mutate(grp2 = ifelse(is.na(lgl), 1L, as.integer(lgl)))
  )

  skip("group_by not yet implemented: https://github.com/voltrondata/substrait-r/issues/28")
  compare_dplyr_binding(
    .input %>%
      group_by(lgl) %>%
      arrange(.by_group = TRUE) %>%
      pull(lgl),
    tbl
  )

  skip("group_by not yet implemented: https://github.com/voltrondata/substrait-r/issues/28")
  compare_dplyr_binding(
    .input %>%
      arrange() %>%
      collect(),
    tbl %>%
      group_by(lgl)
  )

  skip("group_by not yet implemented: https://github.com/voltrondata/substrait-r/issues/28")
  compare_dplyr_binding(
    .input %>%
      group_by(lgl) %>%
      arrange() %>%
      collect(),
    tbl
  )

  skip("error with empty arrange: https://github.com/voltrondata/substrait-r/issues/123")
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      arrange() %>%
      collect(),
    tbl
  )
  test_sort_col <- "chr"
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      arrange(!!sym(test_sort_col)) %>%
      collect(),
    tbl %>%
      select(chr, lgl)
  )
  test_sort_cols <- c("int", "dbl")
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      arrange(!!!syms(test_sort_cols)) %>%
      collect(),
    tbl
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
    tbl
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

  compare_dplyr_binding(
    engine = "duckdb",
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
  expect_error(
    tbl %>%
      arrow_substrait_compiler() %>%
      arrange(desc(int, chr)),
    "expects only one argument",
    fixed = TRUE
  )
})
