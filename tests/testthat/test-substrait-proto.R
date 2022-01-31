
test_that("substrait_proto_message class works", {
  expect_identical(
    substrait$Type$Boolean$create(),
    structure(
      raw(0),
      class = c(
        "substrait_Type_Boolean",
        "substrait_proto_message",
        "substrait_proto"
      )
    )
  )

  expect_output(
    print(substrait$Type$Boolean$create()),
    "substrait.Type.Boolean"
  )
})

test_that("substrait_proto_enum class works", {
  expect_identical(
    substrait$Type$Nullability$NULLABILITY_REQUIRED,
    2
  )

  expect_identical(
    substrait$Type$Nullability$create(2),
    structure(
      2L,
      class = c(
        "substrait_Type_Nullability",
        "substrait_proto_enum",
        "substrait_proto"
      )
    )
  )

  expect_identical(
    substrait$Type$Nullability$create("NULLABILITY_REQUIRED"),
    structure(
      2L,
      class = c(
        "substrait_Type_Nullability",
        "substrait_proto_enum",
        "substrait_proto"
      )
    )
  )

  expect_error(
    substrait$Type$Nullability$create(5L),
    "not a valid identifier"
  )

  expect_error(
    substrait$Type$Nullability$create("NULLABILITY_AWESOME"),
    "not a valid identifier"
  )

  expect_output(
    print(substrait$Type$Nullability$create(2)),
    "substrait.Type.NULLABILITY_REQUIRED"
  )
})
