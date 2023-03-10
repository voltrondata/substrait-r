
test_that("substrait_join() works with the duckdb compiler", {
  skip_if_not(has_duckdb_with_substrait())

  cities <- data.frame(
    city = c("Halifax", "Lancaster", "Chicago"),
    country = c("Canada", "United Kingdom", "United States"),
    stringsAsFactors = FALSE
  )

  countries <- data.frame(
    country = c("United States", "Canada", "United Kingdom"),
    continent = c("North America", "North America", "Europe")
  )

  joined <- substrait_join(
    duckdb_substrait_compiler(cities),
    countries
  )

  expect_identical(
    dplyr::collect(joined) %>% dplyr::arrange(city),
    tibble::tibble(
      city = c("Chicago", "Halifax", "Lancaster"),
      country.x = c("United States", "Canada", "United Kingdom"),
      country.y = c("United States", "Canada", "United Kingdom"),
      continent = c("North America", "North America", "Europe")
    )
  )
})

test_that("substrait_join() works with the arrow compiler", {
  skip_if_not(has_arrow_with_substrait())

  cities <- data.frame(
    city = c("Halifax", "Lancaster", "Chicago"),
    country = c("Canada", "United Kingdom", "United States"),
    stringsAsFactors = FALSE
  )

  countries <- data.frame(
    country = c("United States", "Canada", "United Kingdom"),
    continent = c("North America", "North America", "Europe"),
    stringsAsFactors = FALSE
  )

  joined <- substrait_join(
    arrow_substrait_compiler(cities),
    countries
  )

  # TODO: doesn't actually work for the rhs? (Gives me all zeroes)
  expect_identical(
    colnames(dplyr::collect(joined)),
    c("city", "country.x", "country.y", "continent")
  )
})

test_that("substrait_join() calculates the output schema correctly", {
  df_left <- data.frame(number = 1:26, letter = letters, stringsAsFactors = FALSE)
  df_right <- data.frame(number = 1:26, LETTER = LETTERS, stringsAsFactors = FALSE)

  # With no emit/name repair magic
  joined_all <- substrait_join(
    join_dummy_compiler(df_left),
    df_right,
    name_repair_func = join_name_repair_none(),
    output_mapping_func = join_emit_all()
  )
  expect_identical(
    joined_all$rel$join$common$emit$output_mapping,
    0:3
  )

  expect_identical(
    joined_all$schema$names,
    c("number", "letter", "number", "LETTER")
  )
  expect_identical(
    joined_all$schema$struct$types,
    list(
      substrait_i32(),
      substrait_string(),
      substrait_i32(),
      substrait_string()
    )
  )

  # With all columns and name repair
  joined_suffixed <- substrait_join(
    join_dummy_compiler(df_left),
    df_right,
    name_repair_func = join_name_repair_suffix_common(),
    output_mapping_func = join_emit_all()
  )
  expect_identical(
    joined_suffixed$rel$join$common$emit$output_mapping,
    0:3
  )

  expect_identical(
    joined_suffixed$schema$names,
    c("number.x", "letter", "number.y", "LETTER")
  )
  expect_identical(
    joined_suffixed$schema$struct$types,
    list(
      substrait_i32(),
      substrait_string(),
      substrait_i32(),
      substrait_string()
    )
  )

  # With all left columns and name repair
  joined_default <- substrait_join(
    join_dummy_compiler(df_left),
    df_right,
    name_repair_func = join_name_repair_suffix_common(),
    output_mapping_func = join_emit_default()
  )
  expect_identical(
    joined_default$rel$join$common$emit$output_mapping,
    c(0L, 1L, 3L)
  )

  expect_identical(
    joined_default$schema$names,
    c("number", "letter", "LETTER")
  )
  expect_identical(
    joined_default$schema$struct$types,
    list(
      substrait_i32(),
      substrait_string(),
      substrait_string()
    )
  )
})

test_that("substrait_join() combines data sources correctly", {
  df_left <- data.frame(number = 1:26, letter = letters, stringsAsFactors = FALSE)
  df_right <- data.frame(number = 1:26, LETTER = LETTERS, stringsAsFactors = FALSE)

  join_compiler_df <- substrait_join(join_dummy_compiler(df_left), df_right)
  expect_identical(
    join_compiler_df$rel$join$left$read$base_schema$names,
    names(df_left)
  )
  expect_identical(
    join_compiler_df$rel$join$right$read$base_schema$names,
    names(df_right)
  )

  join_df_compiler <- substrait_join(df_left, join_dummy_compiler(df_right))
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

  local_compiler(join_dummy_compiler(df_sim))

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
    join_emit_all()(NULL, c("a", "b", "c"), c("b", "c", "d")),
    0:5
  )

  expect_identical(
    join_emit_all()(character(), c("a", "b", "c"), c("b", "c", "d")),
    0:5
  )
})

test_that("join_emit_default() omits join keys from the right", {
  expect_identical(
    join_emit_default()(NULL, c("a", "b", "c"), c("b", "c", "d")),
    c(0L, 1L, 2L, 5L)
  )
})

test_that("join_name_repair_none() does not disambiguate column names", {
  expect_identical(
    join_name_repair_none()(0:5, c("a", "b", "c"), c("b", "c", "d")),
    c("a", "b", "c", "b", "c", "d")
  )

  expect_identical(
    join_name_repair_none()(5:0, c("a", "b", "c"), c("b", "c", "d")),
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
