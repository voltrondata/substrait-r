
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
    as_substrait(rlang::quo(!! some_value)),
    as_substrait(5, "substrait.Expression")
  )

  expect_error(
    as_substrait(rlang::quo(stuff), "not.A.Type"),
    "Can't create not.A.Type"
  )
})

test_that("quosures with field references can be translated to Expressions", {
  context <- new_context(data.frame(a = double(), b = character()))

  ref_a <- substrait$Expression$create(
    selection = list(
      direct_reference = list(
        struct_field = list(
          field = 1
        )
      )
    )
  )

  ref_b <- substrait$Expression$create(
    selection = list(
      direct_reference = list(
        struct_field = list(
          field = 2
        )
      )
    )
  )

  expect_identical(as_substrait(rlang::quo(a), context = context), ref_a)
  expect_identical(as_substrait(rlang::quo(.data$a), context = context), ref_a)
  expect_identical(as_substrait(rlang::quo(.data[["a"]]), context = context), ref_a)

  expect_identical(as_substrait(rlang::quo(b), context = context), ref_b)
  expect_identical(as_substrait(rlang::quo(.data$b), context = context), ref_b)
  expect_identical(as_substrait(rlang::quo(.data[["b"]]), context = context), ref_b)
})

test_that("quosures with calls can be translated to Expressions", {
  compiler <- substrait_compiler()
  context <- new_context(data.frame(a = double(), b = character()))

  expect_identical(
    as_substrait(rlang::quo(some_fun(5L)), compiler = compiler, context = context),
    substrait$Expression$create(
      scalar_function = substrait$Expression$ScalarFunction$create(
        function_reference = 1,
        args = list(
          substrait$Expression$create(
            literal = substrait$Expression$Literal$create(i32 = 5L)
          )
        ),
        output_type = substrait$Type$create()
      )
    )
  )

  expect_identical(
    as_substrait(rlang::quo(some_pkg::some_fun(5L)), compiler = compiler, context = context),
    substrait$Expression$create(
      scalar_function = substrait$Expression$ScalarFunction$create(
        function_reference = 2,
        args = list(
          substrait$Expression$create(
            literal = substrait$Expression$Literal$create(i32 = 5L)
          )
        ),
        output_type = substrait$Type$create()
      )
    )
  )

  expect_identical(
    as_substrait(rlang::quo((!! sqrt)(5L))),
    substrait$Expression$create(
      literal = substrait$Expression$Literal$create(
        fp64 = sqrt(5L)
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
    substrait$Type$create(i32 = list())
  )

  ref <- substrait$Expression$create(
    selection = substrait$Expression$FieldReference$create(
      direct_reference = substrait$Expression$ReferenceSegment$create(
        struct_field = substrait$Expression$ReferenceSegment$StructField$create(
          field = 1
        )
      )
    )
  )

  expect_identical(
    as_substrait(
      ref,
      "substrait.Type",
      context = new_context(data.frame(a = 5L))
    ),
    substrait$Type$create(i32 = list())
  )

  expect_error(
    as_substrait(
      ref,
      "substrait.Type",
      context = new_context(data.frame())
    ),
    "Index out of bounds"
  )

  expect_error(
    as_substrait(
      ref,
      "substrait.Type",
      context = list()
    ),
    "Can't guess field reference type without"
  )

  expect_identical(
    as_substrait(
      substrait$Expression$create(
        cast = substrait$Expression$Cast$create(
          type = substrait$Type$create(i32 = list())
        )
      ),
      "substrait.Type"
    ),
    substrait$Type$create(i32 = list())
  )

  expect_identical(
    as_substrait(
      substrait$Expression$create(
        scalar_function = substrait$Expression$ScalarFunction$create(
          output_type = substrait$Type$create(i32 = list())
        )
      ),
      "substrait.Type"
    ),
    substrait$Type$create(i32 = list())
  )

  expect_identical(
    as_substrait(
      substrait$Expression$create(
        window_function = substrait$Expression$WindowFunction$create(
          output_type = substrait$Type$create(i32 = list())
        )
      ),
      "substrait.Type"
    ),
    substrait$Type$create(i32 = list())
  )
})
