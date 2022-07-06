library(dplyr, warn.conflicts = FALSE)
library(arrow)
skip_if_not(has_arrow_with_substrait())

test_that("Empty select returns no columns", {
  skip("Arrow - https://github.com/voltrondata/substrait-r/issues/51")
  compare_dplyr_binding(
    .input %>% select() %>% collect(),
    example_data
  )

  expect_error(
    example_data %>% duckdb_substrait_compiler() %>% select() %>% collect(),
    "Column list must not be empty"
  )

})

test_that("Empty select still includes the group_by columns", {

  expect_message(
    compare_dplyr_binding(
      .input %>% group_by(chr) %>% select() %>% collect(),
      example_data
    ),
    "Adding missing grouping variables"
  )
})

# These have been split out compared to the Arrow test file
test_that("select()", {
  compare_dplyr_binding(
    .input %>%
      select(string = chr, int) %>%
      collect(),
    example_data
  )
})

test_that("rename()", {
  compare_dplyr_binding(
    .input %>%
      rename(string = chr) %>%
      collect(),
    example_data
  )
})

test_that("rename_with", {

  compare_dplyr_binding(
    .input %>%
      rename_with(
        ~ paste0(.x, "_suffix"),
        .cols = c("int", "chr")
      ) %>%
      collect(),
    example_data
  )
})

test_that("select using selection helpers", {
  compare_dplyr_binding(
    .input %>%
      select(everything()) %>%
      collect(),
    example_data
  )

  compare_dplyr_binding(
    .input %>%
      select(any_of(c("int", "not_a_column", "lgl"))) %>%
      collect(),
    example_data
  )

  compare_dplyr_binding(
    .input %>%
      select(starts_with("d")) %>%
      collect(),
    example_data
  )

  compare_dplyr_binding(
    .input %>%
      select(where(is.numeric)) %>%
      collect(),
    example_data
  )
})

test_that("filtering with rename", {

  # skip("https://github.com/voltrondata/substrait-r/issues/92")
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      filter(chr == "b") %>%
      select(string = chr, int) %>%
      collect(),
    example_data
  )

  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      select(string = chr, int) %>%
      filter(string == "b") %>%
      collect(),
    example_data
  )
})

test_that("relocate", {
  skip("Relocate not yet implemented: https://github.com/voltrondata/substrait-r/issues/53")

  compare_dplyr_binding(
    .input %>% relocate(int) %>% collect(),
    example_data
  )

  compare_dplyr_binding(
    .input %>% relocate(int, .after = lgl) %>% collect(),
    example_data
  )

  compare_dplyr_binding(
    .input %>% relocate(int, .before = lgl) %>% collect(),
    example_data
  )

  compare_dplyr_binding(
    .input %>% relocate(int, .after = last_col()) %>% collect(),
    example_data
  )

  compare_dplyr_binding(
    .input %>% relocate(chr2 = chr) %>% collect(),
    example_data
  )
})

test_that("relocate with selection helpers", {
  skip("relocate not yet implemented: https://github.com/voltrondata/substrait-r/issues/53")
  df <- tibble(a = 1, b = 1, c = 1, d = "a", e = "a", f = "a")
  compare_dplyr_binding(
    .input %>% relocate(any_of(c("dbl", "dbl2"))) %>% collect(),
    example_data
  )
  compare_dplyr_binding(
    .input %>% relocate(where(is.character)) %>% collect(),
    example_data
  )
  compare_dplyr_binding(
    .input %>% relocate(int, lgl, .after = where(is.character)) %>% collect(),
    example_data
  )
  compare_dplyr_binding(
    .input %>% relocate(int, lgl, .before = where(is.numeric)) %>% collect(),
    example_data
  )
  # works after other dplyr verbs
  compare_dplyr_binding(
    .input %>%
      mutate(false = as.numeric(false)) %>%
      relocate(int, lgl, .after = where(is.numeric)) %>%
      collect(),
    example_data
  )
})
