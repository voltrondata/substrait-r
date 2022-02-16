
test_that("as_substrait() works for double()", {
  # The substrait.Type representation of a double() is a Type with the
  # fp64 member set and unknown nullability
  expect_identical(
    as_substrait(3.14, "substrait.Type"),
    substrait$Type$create(fp64 = list())
  )

  # The Substrait representation of a non-NA double(1)
  # as an Expression is a Literal with the fp64 member set
  expect_identical(
    as_substrait(3.14, "substrait.Expression"),
    substrait$Expression$create(
      literal = substrait$Expression$Literal$create(fp64 = 3.14)
    )
  )

  # The Substrait representation of an NA double(1)
  # as an Expression is a Literal with the null member set
  # to the appropriate type.
  expect_identical(
    as_substrait(NA_real_),
    substrait$Expression$Literal$create(
      null = substrait$Type$create(fp64 = list())
    )
  )

  # The default representation of a double(1) is a Literal with the
  # fp64 payload set.
  expect_identical(
    as_substrait(3.14),
    substrait$Expression$Literal$create(fp64 = 3.14)
  )

  # The representation of a double(n) is a Literal$List() of Literals
  # (which we can create by lapplying along the double())
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

  # Check that we fail when an unexpected type is requested
  expect_error(as_substrait(3.14, "substrait.NotAType"), "Can't create substrait")
  expect_error(as_substrait(c(3.14, 3.15), "substrait.NotAType"), "Can't create substrait")
})

test_that("from_substrait() works for double()", {
  # Check that we can extract a double() from an Expression with the literal
  # member set.
  expect_identical(
    from_substrait(
      substrait$Expression$create(
        literal = substrait$Expression$Literal$create(fp64 = 3.14)
      ),
      double()
    ),
    3.14
  )

  # Check that we can extract a double() from a Literal with the fp64
  # member set.
  expect_identical(
    from_substrait(
      substrait$Expression$Literal$create(fp64 = 3.14),
      double()
    ),
    3.14
  )

  # Check that we can extract a double() from an Expression with
  # the null member set.
  expect_identical(
    from_substrait(
      substrait$Expression$Literal$create(
        null = substrait$Type$create(fp64 = list())
      ),
      double()
    ),
    NA_real_
  )

  # Check that we can extract a double(n) from a Literal with
  # the list member set.
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

test_that("as_substrait() works for integer()", {
  expect_identical(
    as_substrait(3L, "substrait.Type"),
    substrait$Type$create(i32 = list())
  )

  expect_identical(
    as_substrait(3L, "substrait.Expression"),
    substrait$Expression$create(
      literal = substrait$Expression$Literal$create(i32 = 3)
    )
  )

  expect_identical(
    as_substrait(NA_integer_),
    substrait$Expression$Literal$create(
      null = substrait$Type$create(i32 = list())
    )
  )

  expect_identical(
    as_substrait(3L),
    substrait$Expression$Literal$create(i32 = 3)
  )

  expect_identical(
    as_substrait(c(3L, 4L), substrait$Expression$Literal$create(list = list())),
    substrait$Expression$Literal$create(
      list = substrait$Expression$Literal$List$create(
        list(
          as_substrait(3L),
          as_substrait(4L)
        )
      )
    )
  )

  expect_error(as_substrait(3L, "substrait.NotAType"), "Can't create substrait")
  expect_error(as_substrait(c(3L, 4L), "substrait.NotAType"), "Can't create substrait")
})

test_that("as_substrait() works for logical()", {
  expect_identical(
    as_substrait(TRUE, "substrait.Type"),
    substrait$Type$create(bool_ = list())
  )

  expect_identical(
    as_substrait(TRUE, "substrait.Expression"),
    substrait$Expression$create(
      literal = substrait$Expression$Literal$create(boolean = TRUE)
    )
  )

  expect_identical(
    as_substrait(NA),
    substrait$Expression$Literal$create(
      null = substrait$Type$create(bool_ = list())
    )
  )

  expect_identical(
    as_substrait(TRUE),
    substrait$Expression$Literal$create(boolean = TRUE)
  )

  expect_identical(
    as_substrait(c(TRUE, FALSE), substrait$Expression$Literal$create(list = list())),
    substrait$Expression$Literal$create(
      list = substrait$Expression$Literal$List$create(
        list(
          as_substrait(TRUE),
          as_substrait(FALSE)
        )
      )
    )
  )

  expect_error(as_substrait(TRUE, "substrait.NotAType"), "Can't create substrait")
  expect_error(as_substrait(c(TRUE, FALSE), "substrait.NotAType"), "Can't create substrait")
})

test_that("as_substrait() works for character()", {
  expect_identical(
    as_substrait("a string", "substrait.Type"),
    substrait$Type$create(string = list())
  )

  expect_identical(
    as_substrait("a string", "substrait.Expression"),
    substrait$Expression$create(
      literal = substrait$Expression$Literal$create(string = "a string")
    )
  )

  expect_identical(
    as_substrait(NA_character_),
    substrait$Expression$Literal$create(
      null = substrait$Type$create(string = list())
    )
  )

  expect_identical(
    as_substrait("a string"),
    substrait$Expression$Literal$create(string = "a string")
  )

  expect_identical(
    as_substrait(
      c("a string", "another string"),
      substrait$Expression$Literal$create(list = list())
    ),
    substrait$Expression$Literal$create(
      list = substrait$Expression$Literal$List$create(
        list(
          as_substrait("a string"),
          as_substrait("another string")
        )
      )
    )
  )

  expect_error(as_substrait("", "substrait.NotAType"), "Can't create substrait")
  expect_error(as_substrait(c("", ""), "substrait.NotAType"), "Can't create substrait")
})

test_that("from_substrait() works for integer()", {

  expect_identical(
    from_substrait(
      substrait$Expression$create(
        literal = substrait$Expression$Literal$create(i32 = 3L)
      ),
      integer()
    ),
    3L
  )

  expect_identical(
    from_substrait(
      substrait$Expression$Literal$create(i32 = 3L),
      integer()
    ),
    3L
  )

  expect_identical(
    from_substrait(
      substrait$Expression$Literal$create(
        null = substrait$Type$create(i32 = list())
      ),
      integer()
    ),
    NA_integer_
  )

  expect_identical(
    from_substrait(
      substrait$Expression$Literal$create(
        list = substrait$Expression$Literal$List$create(
          list(
            as_substrait(3L),
            as_substrait(4L)
          )
        )
      ),
      integer()
    ),
    c(3L, 4L)
  )
})

test_that("from_substrait() works for logical()", {

  expect_identical(
    from_substrait(
      substrait$Expression$create(
        literal = substrait$Expression$Literal$create(boolean = TRUE)
      ),
      logical()
    ),
    TRUE
  )

  expect_identical(
    from_substrait(
      substrait$Expression$Literal$create(boolean = TRUE),
      logical()
    ),
    TRUE
  )

  expect_identical(
    from_substrait(
      substrait$Expression$Literal$create(
        null = substrait$Type$create(bool_ = list())
      ),
      logical()
    ),
    NA
  )

  expect_identical(
    from_substrait(
      substrait$Expression$Literal$create(
        list = substrait$Expression$Literal$List$create(
          list(
            as_substrait(TRUE),
            as_substrait(FALSE)
          )
        )
      ),
      logical()
    ),
    c(TRUE, FALSE)
  )
})

test_that("from_substrait() works for character()", {

  expect_identical(
    from_substrait(
      substrait$Expression$create(
        literal = substrait$Expression$Literal$create(string = "a string")
      ),
      character()
    ),
    "a string"
  )

  expect_identical(
    from_substrait(
      substrait$Expression$Literal$create(string = "a string"),
      character()
    ),
    "a string"
  )

  expect_identical(
    from_substrait(
      substrait$Expression$Literal$create(
        null = substrait$Type$create(string = list())
      ),
      character()
    ),
    NA_character_
  )

  expect_identical(
    from_substrait(
      substrait$Expression$Literal$create(
        list = substrait$Expression$Literal$List$create(
          list(
            as_substrait("a string"),
            as_substrait("another string")
          )
        )
      ),
      character()
    ),
    c("a string", "another string")
  )
})

