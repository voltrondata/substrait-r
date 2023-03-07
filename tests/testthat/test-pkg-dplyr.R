
test_that("dplyr::select() for substrait_compiler wraps substrait_select()", {
  skip_if_not_installed("dplyr")

  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)

  result <- dplyr::select(compiler, col1, col2)

  expect_identical(
    dplyr::select(compiler, col1, col2),
    substrait_select(compiler, col1, col2)
  )

  expect_identical(
    dplyr::select(compiler, col1, col2),
    substrait_select(compiler, col1, col2)
  )

  expect_identical(
    dplyr::select(compiler, everything()),
    substrait_select(compiler, col1, col2)
  )

  expect_identical(
    dplyr::select(compiler, where(is.numeric)),
    substrait_select(compiler, col1)
  )
})

test_that("group_by() adds missing groups back in", {
  skip_if_not_installed("dplyr")

  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)

  expect_message(
    compiler %>%
      substrait_group_by(col1) %>%
      select(col2),
    "Adding missing grouping variables: `col1`"
  )
})

test_that("rename() for substrait_compiler renames columns", {
  skip_if_not_installed("dplyr")

  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)

  expect_identical(
    dplyr::rename(compiler, col1_renamed = col1),
    substrait_select(compiler, col1_renamed = col1, col2)
  )
})

test_that("rename_with() for substrait_compiler renames columns using a function", {
  skip_if_not_installed("dplyr")

  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)

  expect_identical(
    dplyr::rename_with(compiler, toupper),
    substrait_select(compiler, "COL1" = col1, "COL2" = col2)
  )
})

test_that("filter() for substrait_compiler wraps substrait_filter()", {
  skip_if_not_installed("dplyr")

  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)
  compiler$.fns[[">"]] <- function(lhs, rhs) substrait_call(">", lhs, rhs)

  expect_identical(
    dplyr::filter(compiler, col1 > 0),
    substrait_filter(compiler, col1 > 0)
  )
})

test_that("mutate() for substrait_compiler wraps substrait_select()", {
  skip_if_not_installed("dplyr")

  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)
  compiler$.fns[[">"]] <- function(lhs, rhs) substrait_call(">", lhs, rhs)

  expect_identical(
    dplyr::mutate(compiler, col1 > 0),
    substrait_select(compiler, col1, col2, col1 > 0)
  )
})

test_that("transmute() for substrait_compiler wraps substrait_select()", {
  skip_if_not_installed("dplyr")

  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)
  compiler$.fns[[">"]] <- function(lhs, rhs) substrait_call(">", lhs, rhs)

  expect_identical(
    dplyr::transmute(compiler, col1 > 0),
    substrait_select(compiler, col1 > 0)
  )
})

test_that("arrange() for substrait_compiler wraps substrait_sort()", {
  skip_if_not_installed("dplyr")

  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)

  expect_identical(
    dplyr::arrange(compiler, col1),
    substrait_sort(compiler, col1)
  )

  expect_identical(
    dplyr::arrange(compiler, desc(col1)),
    substrait_sort(
      compiler,
      substrait_sort_field(col1, "SORT_DIRECTION_DESC_NULLS_LAST")
    )
  )
})

test_that("group_by() for substrait_compiler wraps substrait_group_by()", {
  skip_if_not_installed("dplyr")

  df <- data.frame(
    a = c(1, 1, 2, 2, 3),
    b = c(1, 1, 1, 2, 2),
    c = 1:5
  )

  compiler <- substrait_compiler(df)
  expect_identical(
    dplyr::group_by(compiler, a),
    substrait_group_by(compiler, a)
  )

  grouped <- substrait_group_by(compiler, a)
  expect_identical(
    dplyr::group_by(grouped, b, .add = FALSE),
    substrait_group_by(grouped, b)
  )
  expect_identical(
    dplyr::group_by(grouped, b, .add = TRUE),
    substrait_group_by(grouped, a, b)
  )
})

test_that("ungroup() for substrait_compiler wraps substrait_group_by()", {
  skip_if_not_installed("dplyr")

  df <- data.frame(
    a = c(1, 1, 2, 2, 3),
    b = c(1, 1, 1, 2, 2),
    c = 1:5
  )

  grouped <- substrait_group_by(df, a)
  expect_length(grouped$groups, 1)
  expect_null(dplyr::ungroup(grouped)$groups)
})

test_that("summarise() for substrait_compiler wraps substrait_aggregate()", {
  skip_if_not_installed("dplyr")

  df <- data.frame(
    a = c(1, 1, 2, 2, 3),
    b = c(1, 1, 1, 2, 2),
    c = 1:5
  )

  compiler <- substrait_compiler(df)
  compiler$.fns$sum <- function(x) substrait_call_agg("sum", x)

  expect_identical(
    dplyr::summarise(compiler, sum(c)),
    substrait_aggregate(compiler, sum(c))
  )

  grouped1 <- substrait_group_by(compiler, a)
  expect_identical(
    dplyr::summarise(grouped1, sum(c)),
    substrait_aggregate(grouped1, sum(c))
  )
  expect_identical(
    dplyr::summarise(grouped1, sum(c), .groups = "drop"),
    substrait_aggregate(grouped1, sum(c))
  )
  expect_identical(
    dplyr::summarise(grouped1, sum(c), .groups = "keep"),
    substrait_aggregate(grouped1, sum(c)) %>%
      substrait_group_by(a)
  )

  grouped2 <- substrait_group_by(compiler, a, b)
  expect_identical(
    dplyr::summarise(grouped2, sum(c)),
    substrait_aggregate(grouped2, sum(c)) %>%
      substrait_group_by(a)
  )
  expect_identical(
    dplyr::summarise(grouped2, sum(c), .groups = "drop"),
    substrait_aggregate(grouped2, sum(c))
  )
  expect_identical(
    dplyr::summarise(grouped2, sum(c), .groups = "keep"),
    substrait_aggregate(grouped2, sum(c)) %>%
      substrait_group_by(a, b)
  )
})

test_that("relocate() for substrait_compiler reorders columns", {
  skip_if_not_installed("dplyr")

  tbl <- data.frame(col1 = 1, col2 = "one")
  compiler <- substrait_compiler(tbl)

  expect_identical(
    dplyr::relocate(compiler, col1, .after = col2),
    substrait_select(compiler, col2, col1)
  )
})

test_that("all types of dplyr mutating joins can be created from substrait_comiler", {
  skip_if_not_installed("dplyr")

  df_left <- data.frame(number = 1:26, letter = letters, stringsAsFactors = FALSE)
  df_right <- data.frame(number = 1:26, LETTER = LETTERS, stringsAsFactors = FALSE)
  left <- join_dummy_compiler(df_left)

  # Semi join currently implemented as inner join + emit because support for that
  # appears to be better.
  joined_semi <- dplyr::semi_join(left, df_right)
  expect_s3_class(joined_semi, "SubstraitCompiler")
  expect_identical(
    joined_semi$rel$join$type,
    as.integer(substrait$JoinRel$JoinType$JOIN_TYPE_INNER)
  )
  expect_identical(
    joined_semi$schema$names,
    names(df_left)
  )

  joined_anti <- dplyr::anti_join(left, df_right)
  expect_s3_class(joined_anti, "SubstraitCompiler")
  expect_identical(
    joined_anti$rel$project$input$join$type,
    as.integer(substrait$JoinRel$JoinType$JOIN_TYPE_ANTI)
  )
  expect_identical(
    joined_anti$schema$names,
    names(df_left)
  )
})

test_that("all types of dplyr mutating joins can be created from substrait_comiler", {
  skip_if_not_installed("dplyr")

  df_left <- data.frame(number = 1:26, letter = letters, stringsAsFactors = FALSE)
  df_right <- data.frame(number = 1:26, LETTER = LETTERS, stringsAsFactors = FALSE)
  left <- join_dummy_compiler(df_left)

  joined_left <- dplyr::left_join(left, df_right)
  expect_s3_class(joined_left, "SubstraitCompiler")
  expect_identical(
    joined_left$rel$project$input$join$type,
    as.integer(substrait$JoinRel$JoinType$JOIN_TYPE_LEFT)
  )

  joined_right <- dplyr::right_join(left, df_right)
  expect_s3_class(joined_left, "SubstraitCompiler")
  expect_identical(
    joined_right$rel$project$input$join$type,
    as.integer(substrait$JoinRel$JoinType$JOIN_TYPE_RIGHT)
  )

  joined_inner <- dplyr::inner_join(left, df_right)
  expect_s3_class(joined_left, "SubstraitCompiler")
  expect_identical(
    joined_inner$rel$project$input$join$type,
    as.integer(substrait$JoinRel$JoinType$JOIN_TYPE_INNER)
  )

  joined_full <- dplyr::full_join(left, df_right)
  expect_s3_class(joined_left, "SubstraitCompiler")
  expect_identical(
    joined_full$rel$project$input$join$type,
    as.integer(substrait$JoinRel$JoinType$JOIN_TYPE_OUTER)
  )
})

test_that("dplyr mutating joins for substrait_compiler support by, keep, and suffix", {
  skip_if_not_installed("dplyr")

  df_left <- data.frame(number = 1:26, letter = letters, stringsAsFactors = FALSE)
  df_right <- data.frame(number = 1:26, LETTER = LETTERS, stringsAsFactors = FALSE)

  joined_default <- dplyr::inner_join(
    join_dummy_compiler(df_left),
    df_right
  )

  expect_identical(
    joined_default$schema$names,
    c("number", "letter", "LETTER")
  )

  # keep = TRUE
  joined_keep <- dplyr::inner_join(
    join_dummy_compiler(df_left),
    df_right,
    keep = TRUE
  )

  expect_identical(
    joined_keep$schema$names,
    c("number.x", "letter", "number.y", "LETTER")
  )


  # With custom suffix
  joined_suffixed <- dplyr::inner_join(
    join_dummy_compiler(df_left),
    df_right,
    keep = TRUE,
    suffix = c("_x", "_y")
  )

  expect_identical(
    joined_suffixed$schema$names,
    c("number_x", "letter", "number_y", "LETTER")
  )

  # With custom suffix and keep = FALSE
  df_right2 <- df_right
  df_right2$letter <- letters

  joined_suffixed2 <- dplyr::inner_join(
    join_dummy_compiler(df_left),
    df_right2,
    suffix = c("_x", "_y")
  )

  expect_identical(
    joined_suffixed$schema$names,
    c("number_x", "letter", "number_y", "LETTER")
  )
})
