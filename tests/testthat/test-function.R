
test_that("resolve_function_by_name() works", {
  registry <- new_function_registry()
  extension_dir <- system.file("substrait/extensions", package = "substrait")
  register_functions_yaml(file.path(extension_dir, "functions_arithmetic.yaml"), registry)

  resolved <- resolve_function_by_name("add", list(2, 3), registry)
  expect_s3_class(resolved, "substrait_Expression")
  expect_s3_class(resolved$scalar_function, "substrait_Expression_ScalarFunction")
})

test_that("new_function_registry() works", {
  registry <- new_function_registry()
  expect_setequal(
    names(registry),
    c("scalar", "aggregate", "window", ".next_function_reference")
  )
  expect_s3_class(registry, "substrait_function_registry")
})

test_that("default_function_registry() works", {
  registry <- default_function_registry()
  expect_setequal(
    names(registry),
    c("scalar", "aggregate", "window", ".next_function_reference")
  )
  expect_s3_class(registry, "substrait_function_registry")
})

test_that("register_functions_yaml() works", {
  registry <- new_function_registry()
  extension_dir <- system.file("substrait/extensions", package = "substrait")
  register_functions_yaml(file.path(extension_dir, "unknown.yaml"), registry)

  expect_setequal(
    names(registry$scalar),
    c("subtract", "modulus", "divide", "add", "multiply")
  )

  expect_setequal(
    names(registry$aggregate),
    c("sum", "count", "max", "min", "avg")
  )

  expect_setequal(
    names(registry$window),
    character()
  )
})
