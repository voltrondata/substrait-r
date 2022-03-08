
test_that("substrait_eval_arrow() works", {
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


  result <- substrait_eval_arrow(plan, list(the_name_of_the_table = df))
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

test_that("as_subtrait() works for arrow DataType", {
  skip_if_not_installed("arrow")

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
  skip_if_not_installed("arrow")

  expect_identical(
    as_substrait(arrow::field("a field", arrow::int32())),
    as_substrait(arrow::int32())
  )
})

test_that("as_substrait() works for arrow Schema", {
  skip_if_not_installed("arrow")

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
  skip_if_not_installed("arrow")

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
  skip_if_not_installed("arrow")

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
