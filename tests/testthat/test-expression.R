
test_that("as_substrait() works for double()", {
  expect_identical(
    as_substrait(3.14, "substrait.Expression"),
    substrait$Expression$create(
      literal = substrait$Expression$Literal$create(fp64 = 3.14)
    )
  )

  expect_identical(
    as_substrait(NA_real_),
    substrait$Expression$Literal$create(
      null = substrait$Type$create(fp64 = list())
    )
  )

  expect_identical(
    as_substrait(3.14),
    substrait$Expression$Literal$create(fp64 = 3.14)
  )

  expect_identical(
    as_substrait(c(3.14, 3.15), substrait$Expression$Literal$create(list = list())),
    substrait$Expression$Literal$create(
      list = substrait$Expression$Literal$List$create(
        list(
          as_substrait(3.14),
          as_substrait(3.15)
        )
      )
    )
  )

  expect_error(as_substrait(3.14, "substrait.NotAType"), "Can't create substrait")
  expect_error(as_substrait(c(3.14, 3.15), "substrait.NotAType"), "Can't create substrait")
})

test_that("from_substrait() works for double()", {
  expect_identical(
    from_substrait(
      substrait$Expression$Literal$create(fp64 = 3.14),
      double()
    ),
    3.14
  )

  expect_identical(
    from_substrait(
      substrait$Expression$Literal$create(
        null = substrait$Type$create(fp64 = list())
      ),
      double()
    ),
    NA_real_
  )

  expect_identical(
    from_substrait(
      substrait$Expression$Literal$create(
        list = substrait$Expression$Literal$List$create(
          list(
            as_substrait(3.14),
            as_substrait(3.15)
          )
        )
      ),
      double()
    ),
    c(3.14, 3.15)
  )
})
