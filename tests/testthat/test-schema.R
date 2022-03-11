
test_that("substrait_schema() works for substrait_dplyr_query()", {
  expect_identical(
    substrait_schema(substrait_dplyr_query(data.frame(a_field = integer()))),
    substrait$NamedStruct$create(
      names = "a_field",
      struct_ = substrait$Type$Struct$create(
        types = list(
          substrait_i32()
        )
      )
    )
  )
})


test_that("substrait_schema() works for data.frame()", {
  expect_identical(
    substrait_schema(data.frame(a_field = integer())),
    substrait$NamedStruct$create(
      names = "a_field",
      struct_ = substrait$Type$Struct$create(
        types = list(
          substrait_i32()
        )
      )
    )
  )
})
