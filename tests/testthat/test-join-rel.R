
test_that("substrait_join() combines data sources correctly", {
  # Requires that a translation exists for '==' to generate the join
  # expression, so using duckdb for now
  skip_if_not(has_duckdb_with_substrait())

  df_left <- data.frame(number = 1:26, letter = letters, stringsAsFactors = FALSE)
  df_right <- data.frame(number = 1:26, LETTER = LETTERS, stringsAsFactors = FALSE)

  join_compiler_df <- substrait_join(duckdb_substrait_compiler(df_left), df_right)
  expect_identical(
    join_compiler_df$rel$join$left$read$base_schema$names,
    names(df_left)
  )
  expect_identical(
    join_compiler_df$rel$join$right$read$base_schema$names,
    names(df_right)
  )

  join_df_compiler <- substrait_join(df_left, duckdb_substrait_compiler(df_right))
  expect_identical(
    join_compiler_df$rel$join$left$read$base_schema$names,
    names(df_left)
  )
  expect_identical(
    join_compiler_df$rel$join$right$read$base_schema$names,
    names(df_right)
  )

  expect_error(
    substrait_join(substrait_compiler(df_left), substrait_compiler(df_right)),
    "Merging substrait compilers is not yet implemented"
  )
})

test_that("as_join_expression() can generate join expressions", {
  left <- c("a", "b", "c")
  right <- c("b", "c", "d")
  df_sim <- lapply(c(left, right), function(x) double())
  names(df_sim) <- c(paste0("left.", left), paste0("right.", right))
  df_sim <- as.data.frame(df_sim)

  compiler_with_equals <- substrait_compiler(df_sim)
  compiler_with_equals$.fns[["=="]] <- function(lhs, rhs) {
    substrait_call("==", lhs, rhs)
  }
  compiler_with_equals$.fns[["&"]] <- function(lhs, rhs) {
    substrait_call("&", lhs, rhs)
  }
  local_compiler(compiler_with_equals)

  expect_identical(
    as_join_expression(NULL, left, right),
    as_join_expression(c("b" = "b", "c" = "c"), left, right)
  )

  expect_identical(
    as_join_expression(c("b", "c"), left, right),
    as_join_expression(c("b" = "b", "c" = "c"), left, right)
  )

  expect_identical(
    as_join_expression(c("b" = "b", "c" = "c"), left, right),
    as_substrait(
      substrait_eval_data(left.b == right.b & left.c == right.c),
      "substrait.Expression"
    )
  )

  expect_identical(
    as_join_expression(c("b" = "b"), left, right),
    as_substrait(
      substrait_eval_data(left.b == right.b),
      "substrait.Expression"
    )
  )
})

test_that("join_emit_all() generates a concatenated output mapping", {
  expect_identical(
    join_emit_all(NULL, c("a", "b", "c"), c("b", "c", "d")),
    0:5
  )

  expect_identical(
    join_emit_all(character(), c("a", "b", "c"), c("b", "c", "d")),
    0:5
  )
})

test_that("join_emit_default() omits join keys from the right", {
  expect_identical(
    join_emit_default(NULL, c("a", "b", "c"), c("b", "c", "d")),
    c(0L, 1L, 2L, 5L)
  )
})

test_that("join_name_repair_none() does not disambiguate column names", {
  expect_identical(
    join_name_repair_none(0:5, c("a", "b", "c"), c("b", "c", "d")),
    c("a", "b", "c", "b", "c", "d")
  )

  expect_identical(
    join_name_repair_none(5:0, c("a", "b", "c"), c("b", "c", "d")),
    c("d", "c", "b", "c", "b", "a")
  )
})

test_that("join_name_repair_suffix() suffixes common columns correctly", {
  suffix_default <- join_name_repair_suffix_common(c(".x", ".y"))
  expect_identical(
    suffix_default(0:5, c("a", "b", "c"), c("b", "c", "d")),
    c("a", "b.x", "c.x", "b.y", "c.y", "d")
  )

  expect_identical(
    suffix_default(5:0, c("a", "b", "c"), c("b", "c", "d")),
    c("d", "c.y", "b.y", "c.x", "b.x", "a")
  )

  expect_identical(
    suffix_default(c(0, 1, 2, 5), c("a", "b", "c"), c("b", "c", "d")),
    c("a", "b", "c", "d")
  )
})
