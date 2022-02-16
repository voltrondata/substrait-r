
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
