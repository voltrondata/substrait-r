library(dplyr, warn.conflicts = FALSE)
library(stringr)

tbl <- example_data

example_with_logical_factors <- tibble::tibble(
  starting_a_fight = factor(c(FALSE, TRUE, TRUE, TRUE)),
  consoling_a_child = factor(c(TRUE, FALSE, TRUE, TRUE)),
  petting_a_dog = factor(c(TRUE, TRUE, FALSE, TRUE)),
  saying = c(
    "shhhhh, it's ok",
    "you wanna go outside?",
    "you want your mommy?",
    "hey buddy"
  )
)

test_that("group_by groupings are recorded", {

  compare_dplyr_binding(
    .input %>%
      group_by(chr) %>%
      select(int, chr) %>%
      # skip("comparison operators not implemented yet: https://github.com/voltrondata/substrait-r/issues/92")
      #filter(int > 5) %>%
      collect(),
    tbl
  )
})

test_that("group_by supports creating/renaming", {
  skip("creating/renaming not supported: https://github.com/voltrondata/substrait-r/issues/137")
  compare_dplyr_binding(
    .input %>%
      group_by(chr, numbers = int) %>%
      collect(),
    tbl
  )

  compare_dplyr_binding(
    .input %>%
      group_by(chr, numbers = int * 4) %>%
      collect(),
    tbl
  )
  compare_dplyr_binding(
    .input %>%
      group_by(int > 4, lgl, foo = int > 5) %>%
      collect(),
    tbl
  )
})

test_that("ungroup", {
  compare_dplyr_binding(
    .input %>%
      group_by(chr) %>%
      select(int, chr) %>%
      ungroup() %>%
      # skip("comparison operators not implemented yet: https://github.com/voltrondata/substrait-r/issues/92")
      #filter(int > 5) %>%
      collect(),
    tbl
  )

  # to confirm that the above expectation is actually testing what we think it's
  # testing, verify that compare_dplyr_binding() distinguishes between grouped and
  # ungrouped tibbles
  expect_error(
    compare_dplyr_binding(
      .input %>%
        group_by(chr) %>%
        select(int, chr) %>%
        (function(x) if (inherits(x, "tbl_df")) ungroup(x) else x) %>%
        #filter(int > 5) %>%
        collect(),
      tbl
    )
  )
})

test_that("group_by then rename", {
  skip("creating/renaming not supported: https://github.com/voltrondata/substrait-r/issues/137")
  compare_dplyr_binding(
    .input %>%
      group_by(chr) %>%
      select(string = chr, int) %>%
      collect(),
    tbl
  )
})

test_that("group_by with .drop", {
  skip("factors not yet implemented: https://github.com/voltrondata/substrait-r/issues/138")
  test_groups <- c("starting_a_fight", "consoling_a_child", "petting_a_dog")
  compare_dplyr_binding(
    .input %>%
      group_by(!!!syms(test_groups), .drop = TRUE) %>%
      collect(),
    example_with_logical_factors
  )
  compare_dplyr_binding(
    .input %>%
      group_by(!!!syms(test_groups), .drop = FALSE) %>%
      collect(),
    example_with_logical_factors
  )
  expect_equal(
    example_with_logical_factors %>%
      group_by(!!!syms(test_groups), .drop = TRUE) %>%
      collect() %>%
      n_groups(),
    4L
  )
  expect_equal(
    example_with_logical_factors %>%
      group_by(!!!syms(test_groups), .drop = FALSE) %>%
      collect() %>%
      n_groups(),
    8L
  )
  expect_equal(
    example_with_logical_factors %>%
      group_by(!!!syms(test_groups), .drop = FALSE) %>%
      group_by_drop_default(),
    FALSE
  )
  expect_equal(
    example_with_logical_factors %>%
      group_by(!!!syms(test_groups), .drop = TRUE) %>%
      group_by_drop_default(),
    TRUE
  )
  compare_dplyr_binding(
    .input %>%
      group_by(.drop = FALSE) %>% # no group by vars
      group_by_drop_default(),
    example_with_logical_factors
  )
  compare_dplyr_binding(
    .input %>%
      group_by_drop_default(),
    example_with_logical_factors
  )
  compare_dplyr_binding(
    .input %>%
      group_by(!!!syms(test_groups)) %>%
      group_by_drop_default(),
    example_with_logical_factors
  )
  compare_dplyr_binding(
    .input %>%
      group_by(!!!syms(test_groups), .drop = FALSE) %>%
      ungroup() %>%
      group_by_drop_default(),
    example_with_logical_factors
  )
})

