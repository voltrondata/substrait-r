library(dplyr, warn.conflicts = FALSE)

example_data <- tibble::tibble(
  int = c(1:3, NA_integer_, 5:10),
  dbl = c(1:8, NA, 10) + .1,
  dbl2 = rep(5, 10),
  lgl = sample(c(TRUE, FALSE, NA), 10, replace = TRUE),
  false = logical(10),
  chr = letters[c(1:5, NA, 7:10)],
  fct = factor(letters[c(1:4, NA, NA, 7:10)])
)


test_that("Empty select returns no columns", {
  # compare_dplyr_binding(
  #   .input %>% select() %>% collect(),
  #   tbl
  # )
})

# test_that("Empty select still includes the group_by columns", {
#   expect_message(
#     compare_dplyr_binding(
#       .input %>% group_by(chr) %>% select() %>% collect(),
#       tbl
#     ),
#     "Adding missing grouping variables"
#   )
# })
#
# test_that("select/rename/rename_with", {
#   compare_dplyr_binding(
#     .input %>%
#       select(string = chr, int) %>%
#       collect(),
#     tbl
#   )
#   compare_dplyr_binding(
#     .input %>%
#       rename(string = chr) %>%
#       collect(),
#     tbl
#   )
#   compare_dplyr_binding(
#     .input %>%
#       rename(strng = chr) %>%
#       rename(other = strng) %>%
#       collect(),
#     tbl
#   )
#   compare_dplyr_binding(
#     .input %>%
#       rename_with(
#         ~paste0(.x, "_suffix"),
#         .cols = c("int", "chr")
#       ) %>%
#       collect(),
#     tbl
#   )
#
# })
#
# test_that("select/rename/rename_with using selection helpers", {
#
#   compare_dplyr_binding(
#     .input %>%
#       select(everything()) %>%
#       collect(),
#     tbl
#   )
#   compare_dplyr_binding(
#     .input %>%
#       select(any_of(c("int", "not_a_column", "lgl"))) %>%
#       collect(),
#     tbl
#   )
#
#   compare_dplyr_binding(
#     .input %>%
#       select(starts_with("d")) %>%
#       collect(),
#     tbl
#   )
#   expect_error(
#     compare_dplyr_binding(
#       .input %>%
#         select(where(is.numeric)) %>%
#         collect(),
#       tbl
#     ),
#     "Unsupported selection helper"
#   )
#   compare_dplyr_binding(
#     .input %>%
#       rename_with(toupper) %>%
#       collect(),
#     tbl
#   )
#   compare_dplyr_binding(
#     .input %>%
#       rename_with(toupper, .cols = c()) %>%
#       collect(),
#     tbl
#   )
#   compare_dplyr_binding(
#     .input %>%
#       rename_with(
#         ~paste0(.x, "_suffix"),
#         .cols = starts_with("d")
#       ) %>%
#       collect(),
#     tbl
#   )
# })
#
# test_that("filtering with rename", {
#   compare_dplyr_binding(
#     .input %>%
#       filter(chr == "b") %>%
#       select(string = chr, int) %>%
#       collect(),
#     tbl
#   )
#   compare_dplyr_binding(
#     .input %>%
#       select(string = chr, int) %>%
#       filter(string == "b") %>%
#       collect(),
#     tbl
#   )
# })
#
# test_that("relocate", {
#   df <- tibble(a = 1, b = 1, c = 1, d = "a", e = "a", f = "a")
#   compare_dplyr_binding(
#     .input %>% relocate(f) %>% collect(),
#     df,
#   )
#   compare_dplyr_binding(
#     .input %>% relocate(a, .after = c) %>% collect(),
#     df,
#   )
#   compare_dplyr_binding(
#     .input %>% relocate(f, .before = b) %>% collect(),
#     df,
#   )
#   compare_dplyr_binding(
#     .input %>% relocate(a, .after = last_col()) %>% collect(),
#     df,
#   )
#   compare_dplyr_binding(
#     .input %>% relocate(ff = f) %>% collect(),
#     df,
#   )
# })
#
# test_that("relocate with selection helpers", {
#   df <- tibble(a = 1, b = 1, c = 1, d = "a", e = "a", f = "a")
#   compare_dplyr_binding(
#     .input %>% relocate(any_of(c("a", "e", "i", "o", "u"))) %>% collect(),
#     df
#   )
#   compare_dplyr_binding(
#     .input %>% relocate(where(is.character)) %>% collect(),
#     df
#   )
#   compare_dplyr_binding(
#     .input %>% relocate(a, b, c, .after = where(is.character)) %>% collect(),
#     df
#   )
#   compare_dplyr_binding(
#     .input %>% relocate(d, e, f, .before = where(is.numeric)) %>% collect(),
#     df
#   )
#   # works after other dplyr verbs
#   compare_dplyr_binding(
#     .input %>%
#       mutate(c = as.character(c)) %>%
#       relocate(d, e, f, .after = where(is.numeric)) %>%
#       collect(),
#     df
#   )
# })
