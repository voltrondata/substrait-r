
test_that("quosures of atomics can be translated to Expression objects", {
  expect_identical(
    as_substrait(rlang::quo(5)),
    as_substrait(5, "substrait.Expression")
  )

  some_value <- 5
  expect_identical(
    as_substrait(rlang::quo(some_value)),
    as_substrait(5, "substrait.Expression")
  )

  expect_identical(
    as_substrait(rlang::quo(!!some_value)),
    as_substrait(5, "substrait.Expression")
  )

  expect_error(
    as_substrait(rlang::quo(stuff), "not.A.Type"),
    "Can't create not.A.Type"
  )
})

test_that("quosures with field references can be translated to Expressions", {
  compiler <- local_compiler(data.frame(a = double(), b = character()))

  ref_a <- simple_integer_field_reference(0)
  ref_b <- simple_integer_field_reference(1)

  expect_identical(as_substrait(rlang::quo(a)), ref_a)
  expect_identical(as_substrait(rlang::quo(.data$a)), ref_a)
  expect_identical(as_substrait(rlang::quo(.data[["a"]])), ref_a)

  expect_identical(as_substrait(rlang::quo(b)), ref_b)
  expect_identical(as_substrait(rlang::quo(.data$b)), ref_b)
  expect_identical(as_substrait(rlang::quo(.data[["b"]])), ref_b)
})

test_that("quosures can be translated to SortFields", {
  expect_identical(
    as_substrait(rlang::quo(5L), "substrait.SortField"),
    substrait$SortField$create(
      expr = substrait$Expression$create(
        literal = substrait$Expression$Literal$create(i32 = 5L)
      ),
      direction = "SORT_DIRECTION_ASC_NULLS_LAST"
    )
  )

  expect_identical(
    as_substrait(
      substrait$SortField$create(
        expr = substrait$Expression$create(
          literal = substrait$Expression$Literal$create(i32 = 5L)
        )
      )
    ),
    substrait$SortField$create(
      expr = substrait$Expression$create(
        literal = substrait$Expression$Literal$create(i32 = 5L)
      )
    )
  )
})

test_that("as_substrait() can convert Expression objects to Expressions", {
  expect_identical(
    as_substrait(
      substrait$Expression$create(
        literal = substrait$Expression$Literal$create(i32 = 5)
      )
    ),
    substrait$Expression$create(
      literal = substrait$Expression$Literal$create(i32 = 5)
    )
  )
})

test_that("as_substrait() can convert Expression objects to Types", {
  expect_identical(
    as_substrait(
      substrait$Expression$create(
        literal = substrait$Expression$Literal$create(i32 = 4L)
      ),
      "substrait.Type"
    ),
    substrait_i32()
  )

  with_compiler(substrait_compiler(data.frame(a = 5L)), {
    expect_identical(
      as_substrait(
        simple_integer_field_reference(0L),
        "substrait.Type"
      ),
      substrait_i32()
    )
  })

  with_compiler(substrait_compiler(data.frame()), {
    expect_error(
      as_substrait(
        simple_integer_field_reference(0L),
        "substrait.Type"
      ),
      "Field reference out of bounds"
    )
  })

  expect_error(
    as_substrait(
      simple_integer_field_reference(0L),
      "substrait.Type"
    ),
    "Can't guess field reference type without"
  )

  expect_identical(
    as_substrait(
      substrait$Expression$create(
        cast = substrait$Expression$Cast$create(
          type = substrait_i32()
        )
      ),
      "substrait.Type"
    ),
    substrait_i32()
  )

  expect_identical(
    as_substrait(
      substrait$Expression$create(
        scalar_function = substrait$Expression$ScalarFunction$create(
          output_type = substrait_i32()
        )
      ),
      "substrait.Type"
    ),
    substrait_i32()
  )

  expect_identical(
    as_substrait(
      substrait$Expression$create(
        window_function = substrait$Expression$WindowFunction$create(
          output_type = substrait_i32()
        )
      ),
      "substrait.Type"
    ),
    substrait_i32()
  )
})
