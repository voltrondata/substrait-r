
test_that("as_rprotobuf() works", {
  obj <- structure(
    as.raw(c(0x10, 0x01)),
    class = c(
      "substrait_Type_Boolean",
      "substrait_proto_message",
      "substrait_proto"
    )
  )

  rpb <- as_rprotobuf(obj)
  expect_s4_class(rpb, "Message")
  expect_identical(rpb[["nullability"]], 1L)
})

test_that("as_substrait() works for RProtoBuf Message", {
  expect_identical(
    as_substrait(RProtoBuf::P("substrait.Type.Boolean")$new(nullability = 1)),
    structure(
      as.raw(c(0x10, 0x01)),
      class = c(
        "substrait_Type_Boolean",
        "substrait_proto_message",
        "substrait_proto"
      )
    )
  )

  expect_identical(
    as_substrait(
      RProtoBuf::P("substrait.Type.Boolean")$new(nullability = 1),
      .qualified_name = "substrait.Type.Boolean"
    ),
    structure(
      as.raw(c(0x10, 0x01)),
      class = c(
        "substrait_Type_Boolean",
        "substrait_proto_message",
        "substrait_proto"
      )
    )
  )

  expect_error(
    as_substrait(
      RProtoBuf::P("substrait.Type.Boolean")$new(nullability = 1),
      .qualified_name = "thinger"
    ),
    "is not TRUE"
  )
})