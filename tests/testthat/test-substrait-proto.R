
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

test_that("substrat_proto_message class can be created with a message field", {
  # using internal constructor
  expect_identical(
    substrait$Type$create(bool_ = substrait$Type$Boolean$create()),
    structure(
      as.raw(c(0x0a, 0x00)),
      class = c(
        "substrait_Type",
        "substrait_proto_message",
        "substrait_proto"
      )
    )
  )

  # using RProtoBuf
  expect_identical(
    substrait$Type$create(bool_ = RProtoBuf::P("substrait.Type.Boolean")$new()),
    structure(
      as.raw(c(0x0a, 0x00)),
      class = c(
        "substrait_Type",
        "substrait_proto_message",
        "substrait_proto"
      )
    )
  )

  # using list()
  expect_identical(
    substrait$Type$create(bool_ = list()),
    structure(
      as.raw(c(0x0a, 0x00)),
      class = c(
        "substrait_Type",
        "substrait_proto_message",
        "substrait_proto"
      )
    )
  )

  expect_error(
    substrait$Type$create(i8 = RProtoBuf::P("substrait.Type.Boolean")$new()),
    "wrong message type"
  )
})

test_that("substrait_proto_enum class works", {
  expect_identical(
    substrait$Type$Nullability$NULLABILITY_REQUIRED,
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
