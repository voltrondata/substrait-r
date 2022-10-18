library(dplyr, warn.conflicts = FALSE)
library(stringr)
skip_if_not(has_arrow_with_substrait())

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
      # filter(int > 5) %>%
      collect(),
    example_data
  )
})

test_that("group_by supports creating/renaming", {

  compare_dplyr_binding(
    .input %>%
      group_by(chr, numbers = int) %>%
      collect(),
    example_data
  )

  compare_dplyr_binding(
    .input %>%
      group_by(chr, numbers = int * 4) %>%
      collect(),
    example_data
  )
  compare_dplyr_binding(
    .input %>%
      group_by(int > 4, lgl, foo = int > 5) %>%
      collect(),
    example_data
  )
})

test_that("ungroup", {
  compare_dplyr_binding(
    .input %>%
      group_by(chr) %>%
      select(int, chr) %>%
      ungroup() %>%
      # skip("comparison operators not implemented yet: https://github.com/voltrondata/substrait-r/issues/92")
      # filter(int > 5) %>%
      collect(),
    example_data
  )

  # to confirm that the above expectation is actually testing what we think it's
  # testing, verify that compare_dplyr_binding() distinguishes between grouped and
  # ungrouped tibbles
  expect_error(
    compare_dplyr_binding(
      engine = "duckdb",
      .input %>%
        group_by(chr) %>%
        select(int, chr) %>%
        (function(x) if (inherits(x, "tbl_df")) ungroup(x) else x) %>%
        # filter(int > 5) %>%
        collect(),
      example_data
    )
  )
})

test_that("group_by then rename", {
  compare_dplyr_binding(
    .input %>%
      group_by(chr) %>%
      select(string = chr, int) %>%
      collect(),
    example_data
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

