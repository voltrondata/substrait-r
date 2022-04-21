library(dplyr, warn.conflicts = FALSE)

example_data <- tibble::tibble(
  int = c(1:3, NA_integer_, 5:10),
  dbl = c(1:8, NA, 10) + .1,
  dbl2 = rep(5, 10),
  lgl = sample(c(TRUE, FALSE, NA), 10, replace = TRUE),
  false = logical(10),
  chr = letters[c(1:5, NA, 7:10)],
  #fct = factor(letters[c(1:4, NA, NA, 7:10)])
)

test_that("Empty select returns no columns", {

  skip("https://github.com/voltrondata/substrait-r/issues/51")

  # This doesn't work - see discussion here: https://substrait.slack.com/archives/C02D7CTQXHD/p1650537609741419
  out <- example_data %>%
    arrow_substrait_compiler() %>%
    select() %>%
    collect()

  out <- example_data %>%
    substrait_compiler() %>%
    select()

  plan <- out$plan()

  # i.e. expect that the project rel only has item "named" and not "expressions"
  # write a nice helper function for this?
  # This test is wrong! we should implement this via using the Emit message in RelCommon
  # expect_named(plan[["relations"]][[1]][["rel"]][["project"]], "input")

})

test_that("Empty select still includes the group_by columns", {

  skip("https://github.com/voltrondata/substrait-r/issues/28")

  out <- example_data %>%
    arrow_substrait_compiler() %>%
    group_by(lgl) %>%
    select() %>%
    collect()

  out <- example_data %>%
    substrait_compiler() %>%
    group_by(lgl) %>%
    select()

  plan <- out$plan()

#   expect_message(
#     compare_dplyr_binding(
#       .input %>% group_by(chr) %>% select() %>% collect(),
#       tbl
#     ),
#     "Adding missing grouping variables"
#   )
})

# These have been split out compared to the Arrow test file
test_that("select()", {

  # Select
  out_select <- example_data %>%
    substrait_compiler() %>%
    select(string = chr, int)

  plan_select <- out_select$plan()

  expressions <- plan_select[["relations"]][[1]][["rel"]][["project"]][["expressions"]]

  expect_identical(expressions[[1]], simple_integer_field_reference(5))
  expect_identical(expressions[[2]], simple_integer_field_reference(NULL))
})

test_that("rename", {
  # Rename
  out_rename <- example_data %>%
    substrait_compiler() %>%
    rename(string = chr)

  plan_rename <- out_rename$plan()

  expressions <- plan_rename[["relations"]][[1]][["rel"]][["project"]][["expressions"]]

  # Renaming makes no changes to the plan itself as it only affects the data mask
  expect_identical(expressions[[1]], simple_integer_field_reference(NULL))
  expect_identical(expressions[[2]], simple_integer_field_reference(1))
  expect_identical(expressions[[3]], simple_integer_field_reference(2))
  expect_identical(expressions[[4]], simple_integer_field_reference(3))
  expect_identical(expressions[[5]], simple_integer_field_reference(4))

  # no point testing this as - as above - no visible effects of renaming
  # out_rename2 <- example_data %>%
  #   arrow_substrait_compiler() %>%
  #   rename(strng = chr)  %>%
  #   rename(other = strng) %>%
  #   collect()

})

test_that("rename_with", {
  skip("https://github.com/voltrondata/substrait-r/issues/52")
  out_rename_with <- example_data %>%
    substrait_compiler() %>%
    rename_with(
        ~paste0(.x, "_suffix"),
        .cols = c("int", "chr")
      )
#   compare_dplyr_binding(
#     .input %>%
#       rename_with(
#         ~paste0(.x, "_suffix"),
#         .cols = c("int", "chr")
#       ) %>%
#       collect(),
#     tbl
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


})

test_that("select using selection helpers", {

  # works
  example_data %>%
    arrow_substrait_compiler() %>%
    select(everything()) %>%
    collect()

#   compare_dplyr_binding(
#     .input %>%
#       select(everything()) %>%
#       collect(),
#     tbl
#   )

  # works
  example_data %>%
    arrow_substrait_compiler() %>%
    select(any_of(c("int", "not_a_column", "lgl"))) %>%
    collect()
#   compare_dplyr_binding(
#     .input %>%
#       select(any_of(c("int", "not_a_column", "lgl"))) %>%
#       collect(),
#     tbl
#   )

  # works
  example_data %>%
    arrow_substrait_compiler() %>%
    select(starts_with("d")) %>%
    collect()
#   compare_dplyr_binding(
#     .input %>%
#       select(starts_with("d")) %>%
#       collect(),
#     tbl
#   )

  # unlike in arrow, does work
  example_data %>%
    arrow_substrait_compiler() %>%
    select(where(is.numeric)) %>%
    collect()

#   expect_error(
#     compare_dplyr_binding(
#       .input %>%
#         select(where(is.numeric)) %>%
#         collect(),
#       tbl
#     ),
#     "Unsupported selection helper"
#   )

})

test_that("filtering with rename", {

  skip("substrait function not yet implemented")
  # Doesn't work as `==` function not yet implemented:
  # "Don't know how to convert call to `==` to Arrow"
  example_data %>%
    arrow_substrait_compiler() %>%
    filter(chr == "b") %>%
      select(string = chr, int) %>%
      collect()

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
})

test_that("relocate", {

  skip("https://github.com/voltrondata/substrait-r/issues/53")
  example_data %>%
    arrow_substrait_compiler() %>%
    relocate(chr) %>%
    collect()

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
})

test_that("relocate with selection helpers", {
  skip("https://github.com/voltrondata/substrait-r/issues/53")
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
})
