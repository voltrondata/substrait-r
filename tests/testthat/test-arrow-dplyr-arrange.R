library(dplyr, warn.conflicts = FALSE)
skip_if_not(has_arrow_with_substrait())

skip("dplyr::arrange() doesn't currently work in Arrow via Substrait: https://github.com/voltrondata/substrait-r/issues/68")

example_data_for_sorting <- tibble(
  int = c(-.Machine$integer.max, -101L, -100L, 0L, 0L, 1L, 100L, 1000L, .Machine$integer.max, NA_integer_),
  dbl = c(
    -Inf, -.Machine$double.xmax, -.Machine$double.xmin, 0, .Machine$double.xmin,
    pi, .Machine$double.xmax, Inf, NaN, NA_real_
  ),
  chr = c("", "", "\"", "&", "ABC", "NULL", "a", "abc", "zzz", NA_character_),
  lgl = c(rep(FALSE, 4L), rep(TRUE, 5L), NA),
  # https://github.com/voltrondata/substrait-r/issues/80
  # dttm = lubridate::ymd_hms(c(
  #   "0000-01-01 00:00:00",
  #   "1919-05-29 13:08:55",
  #   "1955-06-20 04:10:42",
  #   "1973-06-30 11:38:41",
  #   "1987-03-29 12:49:47",
  #   "1991-06-11 19:07:01",
  #   NA_character_,
  #   "2017-08-21 18:26:40",
  #   "2017-08-21 18:26:40",
  #   "9999-12-31 23:59:59"
  # )),
  grp = c(rep("A", 5), rep("B", 5))
)


# randomize order of rows in test data
tbl <- slice_sample(example_data_for_sorting, prop = 1L)

test_that("arrange() on integer, double, and character columns", {
  compare_arrow_dplyr_binding(
    .input %>%
      arrange(int, chr) %>%
      collect(),
    tbl
  )
  compare_arrow_dplyr_binding(
    .input %>%
      arrange(int, desc(dbl)) %>%
      collect(),
    tbl
  )
  compare_arrow_dplyr_binding(
    .input %>%
      arrange(int, desc(desc(dbl))) %>%
      collect(),
    tbl
  )
  compare_arrow_dplyr_binding(
    .input %>%
      arrange(int) %>%
      arrange(desc(dbl)) %>%
      collect(),
    tbl
  )
  compare_arrow_dplyr_binding(
    .input %>%
      arrange(int + dbl, chr) %>%
      collect(),
    tbl
  )
  compare_arrow_dplyr_binding(
    .input %>%
      mutate(zzz = int + dbl, ) %>%
      arrange(zzz, chr) %>%
      collect(),
    tbl
  )
  compare_arrow_dplyr_binding(
    .input %>%
      mutate(zzz = int + dbl) %>%
      arrange(int + dbl, chr) %>%
      collect(),
    tbl
  )
  compare_arrow_dplyr_binding(
    .input %>%
      mutate(int + dbl) %>%
      arrange(int + dbl, chr) %>%
      collect(),
    tbl
  )
  compare_arrow_dplyr_binding(
    .input %>%
      group_by(grp) %>%
      arrange(int, dbl) %>%
      collect(),
    tbl
  )
  compare_arrow_dplyr_binding(
    .input %>%
      group_by(grp) %>%
      arrange(int, dbl, .by_group = TRUE) %>%
      collect(),
    tbl
  )
  compare_arrow_dplyr_binding(
    .input %>%
      group_by(grp, grp2) %>%
      arrange(int, dbl, .by_group = TRUE) %>%
      collect(),
    tbl %>%
      mutate(grp2 = ifelse(is.na(lgl), 1L, as.integer(lgl)))
  )
  compare_arrow_dplyr_binding(
    .input %>%
      group_by(grp) %>%
      arrange(.by_group = TRUE) %>%
      pull(grp),
    tbl
  )
  compare_arrow_dplyr_binding(
    .input %>%
      arrange() %>%
      collect(),
    tbl %>%
      group_by(grp)
  )
  compare_arrow_dplyr_binding(
    .input %>%
      group_by(grp) %>%
      arrange() %>%
      collect(),
    tbl
  )
  compare_arrow_dplyr_binding(
    .input %>%
      arrange() %>%
      collect(),
    tbl
  )
  test_sort_col <- "chr"
  compare_arrow_dplyr_binding(
    .input %>%
      arrange(!!sym(test_sort_col)) %>%
      collect(),
    tbl %>%
      select(chr, lgl)
  )
  test_sort_cols <- c("int", "dbl")
  compare_arrow_dplyr_binding(
    .input %>%
      arrange(!!!syms(test_sort_cols)) %>%
      collect(),
    tbl
  )
})

test_that("arrange() on datetime columns", {
  compare_arrow_dplyr_binding(
    .input %>%
      arrange(dttm, int) %>%
      collect(),
    tbl
  )
  compare_arrow_dplyr_binding(
    .input %>%
      arrange(dttm) %>%
      collect(),
    tbl %>%
      select(dttm, grp)
  )
})

test_that("arrange() on logical columns", {
  compare_arrow_dplyr_binding(
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
