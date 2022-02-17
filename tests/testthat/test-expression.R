
test_that("quosures can be translated to Expression objects", {
  expect_identical(
    as_substrait(rlang::quo(5)),
    as_substrait(5, "substrait.Expression")
  )

  some_value <- 5
  expect_identical(
    as_substrait(rlang::quo(some_value)),
    as_substrait(5, "substrait.Expression")
  )

  expect_error(
    as_substrait(rlang::quo(stuff), "not.A.Type"),
    "Can't create not.A.Type"
  )
})

test_that("calls can be translated to Expression objects", {
  registry <- new_function_registry()
  extension_dir <- system.file("substrait/extensions", package = "substrait")
  register_functions_yaml(file.path(extension_dir, "functions_arithmetic.yaml"), registry)

  resolved <- as_substrait(quote(add(5, 5)), functions = registry)
  expect_s3_class(resolved, "substrait_Expression")
  expect_s3_class(resolved$scalar_function, "substrait_Expression_ScalarFunction")

  expect_error(
    as_substrait(quote(pkg::not_a_function())),
    "Can't resolve function"
  )

  expect_error(
    as_substrait(quote(not_a_function())),
    "No such function"
  )

  expect_error(
    as_substrait(call("something"), "not.A.Type"),
    "Can't create not.A.Type"
  )
})

test_that("symbols can be translated to expression objects", {
  expect_identical(
    as_substrait(as.symbol("sym"), schema = list(sym = "some_field")),
    "some_field"
  )

  expect_identical(
    as_substrait(as.symbol("sym"), env = as.environment(list(sym = 5))),
    as_substrait(5, "substrait.Expression")
  )

  expect_error(
    as_substrait(as.symbol("sym"), "not.A.Type"),
    "Can't create not.A.Type"
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
