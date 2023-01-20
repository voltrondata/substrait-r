skip_if_not(has_arrow_with_substrait())

test_that("ArrowSubstraitCompiler$plan() generates the correct extension URIs", {
  df <- tibble::tibble(x = 1:3)

  compiler <- arrow_substrait_compiler(df) %>%
    substrait_select(x1 = x > 2, x2 = x + 2)

  plan <- compiler$plan()
  expect_length(plan$extension_uris, 2)

  expect_identical(plan$extensions[[1]]$extension_function$name, "gt")
  expect_identical(
    plan$extensions[[1]]$extension_function$extension_uri_reference,
    # uri reference for "add"
    2
  )

  expect_identical(plan$extensions[[2]]$extension_function$name, "add")
  expect_identical(
    plan$extensions[[2]]$extension_function$extension_uri_reference,
    # uri reference for "comparison"
    1
  )

  out_df <- as.data.frame(compiler$evaluate())

  expect_identical(
    out_df,
    tibble::tibble(x1 = c(FALSE, FALSE, TRUE), x2 = c(3, 4, 5))
  )
})

test_that("substrait_compiler() creates an ArrowSubstraitCompiler for ArrowTabular", {
  rb <- arrow::record_batch(
    a_field = arrow::Array$create(integer(), arrow::int32())
  )

  compiler <- substrait_compiler(rb)

  expect_identical(
    compiler$schema,
    substrait$NamedStruct$create(
      names = "a_field",
      struct = substrait$Type$Struct$create(
        types = list(
          substrait_i32()
        )
      )
    )
  )

  expect_identical(
    compiler$named_table("named_table_1"),
    rb
  )
})

test_that("ArrowSubstraitCompiler can evaluate a plan with one relation", {
  skip_if_not(has_arrow_with_substrait())

  df <- data.frame(
    letter = letters[1:5],
    number = 1:5
  )

  result <- arrow_substrait_compiler(df)$evaluate()
  expect_identical(as.data.frame(df), df)
})


test_that("ArrowSubstraitCompiler can evaluate a plan with a field reference", {
  skip_if_not(has_arrow_with_substrait())

  df <- data.frame(
    letter = letters[1:5],
    number = 1:5
  )

  compiler <- arrow_substrait_compiler(df)
  result <- substrait_select(compiler, number)

  expect_identical(
    as.data.frame(as.data.frame(result$evaluate())),
    df["number"]
  )
})

test_that("ArrowSubstraitCompiler can evaluate a project with a function call", {
  skip_if_not(has_arrow_with_substrait())

  df <- data.frame(
    letter = letters[1:5],
    number = 1:5
  )

  compiler <- arrow_substrait_compiler(df)
  result <- substrait_select(compiler, added = number + 1L)

  expect_identical(
    as.data.frame(as.data.frame(result$evaluate())),
    data.frame(added = 2:6)
  )
})

test_that("as_subtrait() works for arrow DataType", {
  expect_identical(
    as_substrait(arrow::bool()),
    substrait_boolean()
  )

  expect_identical(
    as_substrait(arrow::int32()),
    substrait_i32()
  )

  expect_identical(
    as_substrait(arrow::float64()),
    substrait_fp64()
  )

  expect_identical(
    as_substrait(arrow::string()),
    substrait_string()
  )

  expect_error(
    as_substrait(arrow::list_of(arrow::bool())),
    "Can't map Arrow DataType"
  )

  expect_error(
    as_substrait(arrow::bool(), "not.A.Type"),
    "Can't create not.A.Type"
  )
})

test_that("as_subtrait() works for arrow Field", {
  expect_identical(
    as_substrait(arrow::field("a field", arrow::int32())),
    as_substrait(arrow::int32())
  )
})

test_that("as_substrait() works for arrow Schema", {
  expect_identical(
    as_substrait(arrow::schema("a field" = arrow::int32())),
    substrait$NamedStruct$create(
      names = "a field",
      struct = substrait$Type$Struct$create(
        types = list(
          substrait_i32()
        )
      )
    )
  )
})

test_that("from_substrait() works for arrow DataType", {
  expect_true(
    from_substrait(substrait$Type$create(), arrow::null()) ==
      arrow::null()
  )
  expect_true(
    from_substrait(substrait_boolean(), arrow::null()) ==
      arrow::bool()
  )
  expect_true(
    from_substrait(substrait_i32(), arrow::null()) ==
      arrow::int32()
  )
  expect_true(
    from_substrait(substrait_fp64(), arrow::null()) ==
      arrow::float64()
  )
  expect_true(
    from_substrait(substrait_string(), arrow::null()) ==
      arrow::string()
  )

  expect_error(
    from_substrait(substrait_uuid(), arrow::null()),
    "Can't convert substrait.Type"
  )
  expect_error(
    from_substrait(substrait$AggregateFunction$create(), arrow::null()),
    "Can't restore"
  )
})

test_that("from_substrait() works for arrow::schema()", {
  expect_true(
    from_substrait(
      substrait$NamedStruct$create(
        names = "a_field",
        struct = substrait$Type$Struct$create(
          types = list(
            substrait_i32()
          )
        )
      ),
      arrow::schema(a_field = arrow::int32())
    ) ==
      arrow::schema(a_field = arrow::int32())
  )

  expect_true(
    from_substrait(
      substrait$NamedStruct$create(
        names = "a_field",
        struct = substrait$Type$Struct$create(
          types = list(
            substrait_i32()
          )
        )
      ),
      arrow::schema()
    ) ==
      arrow::schema(a_field = arrow::int32())
  )
})

test_that("as_substrait() works for ArrowTabular", {
  rb <- arrow::record_batch(
    a_field = arrow::Array$create(integer(), arrow::int32())
  )

  expect_identical(
    as_substrait(rb, "substrait.NamedStruct"),
    as_substrait(rb$schema)
  )

  expect_error(
    as_substrait(rb, "substrait.NotAType"),
    "Can't create substrait.NotAType"
  )
})

test_that("from_substrait() works for RecordBatch", {
  rb <- arrow::record_batch(
    a_field = arrow::Array$create(integer(), arrow::int32())
  )

  recreated_rb <- from_substrait(
    substrait$NamedStruct$create(
      names = "a_field",
      struct = substrait$Type$Struct$create(
        types = list(
          substrait_i32()
        )
      )
    ),
    arrow::record_batch(data.frame())
  )

  expect_true(rb == rb)
})

test_that("substrait_eval_arrow() can evaluate a plan with one read relation", {
  skip_if_not(has_arrow_with_substrait())

  df <- data.frame(
    letter = letters[1:5],
    number = 1:5
  )

  plan <- substrait$Plan$create(
    relations = list(
      substrait$PlanRel$create(
        rel = substrait$Rel$create(
          read = substrait$ReadRel$create(
            base_schema = as_substrait(df, "substrait.NamedStruct"),
            named_table = substrait$ReadRel$NamedTable$create(
              names = "the_name_of_the_table"
            )
          )
        )
      )
    )
  )

  result <- substrait_eval_arrow(
    plan,
    list(the_name_of_the_table = df),
    c("letter", "number")
  )

  expect_identical(as.data.frame(as.data.frame(result)), df)

  expect_snapshot(
    substrait_eval_arrow(plan, list()),
    error = TRUE
  )

  expect_snapshot(
    substrait_eval_arrow(plan, list(the_name_of_the_table = data.frame())),
    error = TRUE
  )
})

test_that("arrow translation for != works", {
  skip_if_not(has_arrow_with_substrait())

  expect_equal(
    example_data[1:5, "dbl"] %>%
      arrow_substrait_compiler() %>%
      substrait_filter(dbl != 0) %>%
      dplyr::collect(),
    tibble::tibble(dbl = c(-999, -99, -9, 9))
  )
})

test_that("arrow translation for == works", {
  skip_if_not(has_arrow_with_substrait())

  expect_equal(
    example_data[1:5, "dbl"] %>%
      arrow_substrait_compiler() %>%
      substrait_filter(dbl == 0) %>%
      dplyr::collect(),
    tibble::tibble(dbl = 0)
  )
})

test_that("arrow translation for < works", {
  skip_if_not(has_arrow_with_substrait())

  expect_equal(
    example_data[1:5, "dbl"] %>%
      arrow_substrait_compiler() %>%
      substrait_filter(dbl < 0) %>%
      dplyr::collect(),
    tibble::tibble(dbl = c(-999, -99, -9))
  )
})

test_that("arrow translation for > works", {
  skip_if_not(has_arrow_with_substrait())

  expect_equal(
    example_data[1:5, "dbl"] %>%
      arrow_substrait_compiler() %>%
      substrait_filter(dbl > 0) %>%
      dplyr::collect(),
    tibble::tibble(dbl = 9)
  )
})

test_that("arrow translation for <= works", {
  skip_if_not(has_arrow_with_substrait())

  expect_equal(
    example_data[1:5, "dbl"] %>%
      arrow_substrait_compiler() %>%
      substrait_filter(dbl <= 0) %>%
      dplyr::collect(),
    tibble::tibble(dbl = c(-999, -99, -9, 0))
  )
})

test_that("arrow translation for >= works", {
  skip_if_not(has_arrow_with_substrait())

  expect_equal(
    example_data[1:5, "dbl"] %>%
      arrow_substrait_compiler() %>%
      substrait_filter(dbl >= 0) %>%
      dplyr::collect(),
    tibble::tibble(dbl = c(0, 9))
  )
})
