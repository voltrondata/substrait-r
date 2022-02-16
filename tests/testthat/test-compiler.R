
test_that("substrait_compiler can be created", {
  compiler <- substrait_compiler()
  expect_s3_class(compiler, "substrait_compiler")
  expect_s3_class(compiler$extension_uri, "substrait_extensions_SimpleExtensionURI")
})

test_that("substrait_compiler_function_id() works", {
  compiler <- substrait_compiler()
  expect_equal(
    substrait_compiler_function_id(compiler, "some_fun", list(), pkg = "base"),
    1L
  )
  expect_equal(
    substrait_compiler_function_id(compiler, "some_fun", list(), pkg = "base"),
    1L
  )

  # another package should trigger a new id
  expect_equal(
    substrait_compiler_function_id(
      compiler,
      "some_fun",
      list(),
      pkg = NULL
    ),
    2L
  )

  # different arg types should trigger a new id
  expect_equal(
    substrait_compiler_function_id(
      compiler,
      "some_fun",
      list(
        substrait$Type$create(i32 = list())
      ),
      pkg = NULL
    ),
    3L
  )

  # ...but doing it again should get the same id
  expect_equal(
    substrait_compiler_function_id(
      compiler,
      "some_fun",
      list(
        substrait$Type$create(i32 = list())
      ),
      pkg = NULL
    ),
    3L
  )
})

test_that("substrait_compiler_next_id() works", {
  compiler <- substrait_compiler()
  expect_identical(substrait_compiler_next_id(compiler), 1L)
  expect_identical(substrait_compiler_next_id(compiler), 2L)
})


