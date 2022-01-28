
test_that("at least a few types-generated constructors work", {
  expect_s3_class(
    substrait$Type$Boolean$create(),
    "substrait_proto_message"
  )

  expect_identical(
    substrait$Type$Nullability$NULLABILITY_REQUIRED,
    2
  )

  expect_s3_class(substrait$Type$Nullability$create(2), "substrait_proto_value")
})
