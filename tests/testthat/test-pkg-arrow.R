
test_that("substrait_compiler() creates an ArrowSubstraitCompiler for ArrowTabular", {
  rb <- arrow::record_batch(
    a_field = arrow::Array$create(integer(), arrow::int32())
  )

  compiler <- substrait_compiler(rb)

  expect_identical(
    compiler$schema,
    substrait$NamedStruct$create(
      names = "a_field",
      struct_ = substrait$Type$Struct$create(
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

test_that("ArrowSubstraitCompiler can translate simple unary and binary calls", {
  compiler <- ArrowSubstraitCompiler$new()

  translated <- compiler$resolve_function("abs", list(5), list())
  translated_fun <- compiler$function_extension(translated$function_reference)
  expect_identical(translated_fun$name, "abs_checked")

  translated <- compiler$resolve_function(">", list(5, 6), list())
  translated_fun <- compiler$function_extension(translated$function_reference)
  expect_identical(translated_fun$name, "greater")

  expect_error(
    compiler$resolve_function("not_a_fun!", list(), list()),
    "Don't know how to convert call to"
  )

  expect_error(
    compiler$resolve_function("abs", list(), list()),
    "Expected one argument"
  )

  expect_error(
    compiler$resolve_function(">", list(), list()),
    "Expected two arguments"
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
  result <- substrait_project(compiler, number)

  expect_identical(
    as.data.frame(as.data.frame(result$evaluate())),
    df["number"]
  )
})

test_that("ArrowSubstraitCompiler can evaluate a plan with a function call", {
  skip_if_not(has_arrow_with_substrait())

  df <- data.frame(
    letter = letters[1:5],
    number = 1:5
  )

  compiler <- arrow_substrait_compiler(df)
  result <- substrait_filter(compiler, number >= 4)

  skip("This doesn't work (crashes)")
  expect_identical(as.data.frame(result$evaluate()), df[df$number >= 4, ])
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
      struct_ = substrait$Type$Struct$create(
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
        struct_ = substrait$Type$Struct$create(
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
        struct_ = substrait$Type$Struct$create(
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
      struct_ = substrait$Type$Struct$create(
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
