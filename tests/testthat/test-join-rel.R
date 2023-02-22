
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
