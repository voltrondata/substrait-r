
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
