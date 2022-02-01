
test_that("as_substrait() default error works", {
  expect_error(as_substrait(3), "Can't create substrait message")
  expect_error(
    as_substrait(3, .qualified_name = "substrait.Type"),
    "Can't create substrait.Type"
  )
})

test_that("from_substrait() default errors work", {
  expect_error(
    from_substrait(3, list()),
    "is not TRUE"
  )

  expect_error(
    from_substrait(substrait$Type$Boolean$create(), environment()),
    "Can't restore substrait.Type.Boolean"
  )
})

test_that("as_substrait() works for list()", {
  msg <- as_substrait(list(nullability = 1), "substrait.Type.Boolean")
  expect_identical(
    as_substrait(list(nullability = 1), "substrait.Type.Boolean"),
    structure(
      as.raw(c(0x10, 0x01)),
      class = c(
        "substrait_Type_Boolean",
        "substrait_proto_message",
        "substrait_proto"
      )
    )
  )

  # check a recursive list
  expect_identical(
    as_substrait(list(i8 = list()), "substrait.Type"),
    substrait$Type$create(i8 = substrait$Type$I8$create())
  )
})

test_that("from_substrait() works for list()", {
  msg <- substrait$Type$create(i8 = substrait$Type$Boolean$create())
  expect_identical(
    from_substrait(msg, list()),
    list(
      i8 = substrait$Type$I8$create()
    )
  )

  expect_identical(
    from_substrait(msg, list(), recursive = TRUE),
    list(
      i8 = rlang::set_names(list(), character())
    )
  )
})

test_that("substrait_proto_message class works", {
  expect_identical(
    substrait$Type$Boolean$create(nullability = 1),
    structure(
      as.raw(c(0x10, 0x01)),
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

  expect_error(
    substrait$Type$create(i8 = environment()),
    "Can't create substrait.Type.I8"
  )
})

test_that("repeated message values work", {
  expect_identical(
    clean_value(
      list(),
      "TYPE_MESSAGE",
      "substrait.Type.Boolean",
      repeated = TRUE
    ),
    list()
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
    substrait$Type$Nullability$create(logical(1)),
    "Expected character identifier or integer"
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
    "substrait.Type.Nullability"
  )
})

test_that("substrait proto enum class can handle multiple values", {
  expect_identical(
    substrait$Type$Nullability$create(0:2),
    structure(
      0:2,
      class = c(
        "substrait_Type_Nullability",
        "substrait_proto_enum",
        "substrait_proto"
      )
    )
  )
})
