
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
