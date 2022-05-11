library(dplyr, warn.conflicts = FALSE)
skip_if_not(has_arrow_with_substrait())

skip("dplyr::arrange() doesn't currently work in Arrow via Substrait: https://github.com/voltrondata/substrait-r/issues/68")

# randomize order of rows in test data
tbl <- slice_sample(example_data, prop = 1L)

test_that("arrange() on integer, double, and character columns", {
  compare_dplyr_binding(
    .input %>%
      arrange(int, chr) %>%
      collect(),
    tbl
  )
  compare_dplyr_binding(
    .input %>%
      arrange(int, desc(dbl)) %>%
      collect(),
    tbl
  )
  compare_dplyr_binding(
    .input %>%
      arrange(int, desc(desc(dbl))) %>%
      collect(),
    tbl
  )
  compare_dplyr_binding(
    .input %>%
      arrange(int) %>%
      arrange(desc(dbl)) %>%
      collect(),
    tbl
  )
  compare_dplyr_binding(
    .input %>%
      arrange(int + dbl, chr) %>%
      collect(),
    tbl
  )
  compare_dplyr_binding(
    .input %>%
      mutate(zzz = int + dbl, ) %>%
      arrange(zzz, chr) %>%
      collect(),
    tbl
  )
  compare_dplyr_binding(
    .input %>%
      mutate(zzz = int + dbl) %>%
      arrange(int + dbl, chr) %>%
      collect(),
    tbl
  )
  compare_dplyr_binding(
    .input %>%
      mutate(int + dbl) %>%
      arrange(int + dbl, chr) %>%
      collect(),
    tbl
  )
  compare_dplyr_binding(
    .input %>%
      group_by(lgl) %>%
      arrange(int, dbl) %>%
      collect(),
    tbl
  )
  compare_dplyr_binding(
    .input %>%
      group_by(lgl) %>%
      arrange(int, dbl, .by_group = TRUE) %>%
      collect(),
    tbl
  )
  compare_dplyr_binding(
    .input %>%
      group_by(lgl, grp2) %>%
      arrange(int, dbl, .by_group = TRUE) %>%
      collect(),
    tbl %>%
      mutate(grp2 = ifelse(is.na(lgl), 1L, as.integer(lgl)))
  )
  compare_dplyr_binding(
    .input %>%
      group_by(lgl) %>%
      arrange(.by_group = TRUE) %>%
      pull(lgl),
    tbl
  )
  compare_dplyr_binding(
    .input %>%
      arrange() %>%
      collect(),
    tbl %>%
      group_by(lgl)
  )
  compare_dplyr_binding(
    .input %>%
      group_by(lgl) %>%
      arrange() %>%
      collect(),
    tbl
  )
  compare_dplyr_binding(
    .input %>%
      arrange() %>%
      collect(),
    tbl
  )
  test_sort_col <- "chr"
  compare_dplyr_binding(
    .input %>%
      arrange(!!sym(test_sort_col)) %>%
      collect(),
    tbl %>%
      select(chr, lgl)
  )
  test_sort_cols <- c("int", "dbl")
  compare_dplyr_binding(
    .input %>%
      arrange(!!!syms(test_sort_cols)) %>%
      collect(),
    tbl
  )
})

test_that("arrange() on datetime columns", {
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
    .input %>%
      arrange(lgl, int) %>%
      collect(),
    tbl
  )
})

test_that("arrange() with bad inputs", {
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
