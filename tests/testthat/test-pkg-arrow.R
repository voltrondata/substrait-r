
test_that("as_subtrait() works for arrow DataType", {
  skip_if_not_installed("arrow")

  expect_identical(
    as_substrait(arrow::bool()),
    substrait$Type$create(bool_ = list())
  )

  expect_identical(
    as_substrait(arrow::int32()),
    substrait$Type$create(i32 = list())
  )

  expect_identical(
    as_substrait(arrow::float64()),
    substrait$Type$create(fp64 = list())
  )

  expect_identical(
    as_substrait(arrow::string()),
    substrait$Type$create(string = list())
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
          substrait$Type$create(i32 = list())
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
    from_substrait(substrait$Type$create(bool_ = list()), arrow::null()) ==
    arrow::bool()
  )
  expect_true(
    from_substrait(substrait$Type$create(i32 = list()), arrow::null()) ==
    arrow::int32()
  )
  expect_true(
    from_substrait(substrait$Type$create(fp64 = list()), arrow::null()) ==
    arrow::float64()
  )
  expect_true(
    from_substrait(substrait$Type$create(string = list()), arrow::null()) ==
    arrow::string()
  )

  expect_error(
    from_substrait(substrait$Type$create(uuid = list()), arrow::null()),
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
            substrait$Type$create(i32 = list())
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
            substrait$Type$create(i32 = list())
          )
        )
      ),
      arrow::schema()
    ) ==
      arrow::schema(a_field = arrow::int32())
  )
})
