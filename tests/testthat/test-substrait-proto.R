
test_that("as_substrait() default error works", {
  expect_error(as_substrait(new.env()), "Can't create substrait message")
  expect_error(
    as_substrait(new.env(), .ptype = "substrait.Type"),
    "Can't create substrait.Type"
  )
  expect_error(
    as_substrait(new.env(), .ptype = make_ptype("substrait.Type")),
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

test_that("substrait_create() works for substrait_proto_auto()", {
  expect_identical(
    substrait_create("substrait.Type.Boolean", nullability = 1),
    structure(
      list(content = as.raw(c(0x10, 0x01))),
      class = c(
        "substrait_Type_Boolean",
        "substrait_proto_message",
        "substrait_proto"
      )
    )
  )

  # check a recursive list
  expect_identical(
    substrait_create("substrait.Type", i8 = substrait_proto_auto()),
    substrait_i8()
  )
})

test_that("as.list() works for substrait objects", {
  msg <- substrait_i8()
  expect_identical(
    as.list(msg),
    list(
      i8 = substrait$Type$I8$create()
    )
  )

  expect_identical(
    as.list(msg, recursive = TRUE),
    list(
      i8 = rlang::set_names(list(), character())
    )
  )

  # check repeated message values
  lst <- as.list(
    substrait$Expression$Literal$List$create(
      values = list(
        substrait$Expression$Literal$create(i32 = 5L)
      )
    )
  )

  expect_identical(
    lst$values[[1]],
    substrait$Expression$Literal$create(i32 = 5L)
  )
})

test_that("as_substrait() works for substrait", {
  expect_identical(
    as_substrait(substrait$Type$Boolean$create()),
    substrait$Type$Boolean$create()
  )

  expect_identical(
    as_substrait(substrait$Type$Boolean$create(), "substrait.Type.Boolean"),
    substrait$Type$Boolean$create()
  )

  expect_error(
    as_substrait(substrait$Type$Boolean$create(), "substrait.Type.Awesome"),
    "is not TRUE"
  )
})

test_that("from_substrait() works for substrait", {
  expect_identical(
    from_substrait(substrait$Type$Boolean$create(), substrait$Type$Boolean$create()),
    substrait$Type$Boolean$create()
  )

  expect_error(
    from_substrait(substrait$Type$Boolean$create(), substrait$Type$I8$create()),
    "is not TRUE"
  )
})

test_that("substrait_proto_message class works", {
  expect_identical(
    substrait$Type$Boolean$create(nullability = 1),
    structure(
      list(content = as.raw(c(0x10, 0x01))),
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

test_that("substrait_proto_message list-like interface works", {
  msg <- substrait$Type$Boolean$create()
  expect_identical(names(msg), character())
  expect_identical(length(msg), 0L)

  msg$nullability <- 1L
  expect_identical(names(msg), "nullability")
  expect_identical(msg$nullability, 1L)
  expect_identical(length(msg), 1L)

  msg[["type_variation_reference"]] <- 393
  expect_identical(names(msg), c("type_variation_reference", "nullability"))
  expect_identical(msg[["type_variation_reference"]], 393)
})

test_that("substrait_proto_message class can be created with a message field", {
  # using internal constructor
  expect_identical(
    substrait_boolean(),
    structure(
      list(content = as.raw(c(0x0a, 0x00))),
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
      list(content = as.raw(c(0x0a, 0x00))),
      class = c(
        "substrait_Type",
        "substrait_proto_message",
        "substrait_proto"
      )
    )
  )

  # using list()
  expect_identical(
    substrait_boolean(),
    structure(
      list(content = as.raw(c(0x0a, 0x00))),
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

  expect_error(
    clean_value(
      raw(),
      "TYPE_MESSAGE",
      "substrait.Type.Boolean",
      repeated = TRUE
    ),
    "must be wrapped in `list"
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
