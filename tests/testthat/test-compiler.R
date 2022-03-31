
test_that("substrait_compiler can be created", {
  compiler <- substrait_compiler()
  expect_s3_class(compiler, "substrait_compiler")
  expect_s3_class(compiler$extension_uri, "substrait_extensions_SimpleExtensionURI")
})

test_that("default substrait_compiler_read_rel() implementation works", {
  compiler <- substrait_compiler()

  # Previously-created ReadRel gets passed through as-is
  read_rel <- substrait$ReadRel$create()
  expect_identical(
    substrait_compiler_read_rel(compiler, read_rel),
    read_rel
  )

  # All other objects are have their schemas extracted and are kept track of
  tbl <- data.frame(a = 1L, b = "one")
  read_rel <- expect_s3_class(
    substrait_compiler_read_rel(compiler, tbl),
    "substrait_ReadRel"
  )

  expect_match(read_rel$named_table$names, "^named_table_")
  expect_identical(
    compiler$named_tables[[read_rel$named_table$names]],
    tbl
  )
})

test_that("substrait_compiler_function_id() works", {
  compiler <- substrait_compiler()
  expect_equal(
    substrait_compiler_function_id(compiler, "some_fun", list()),
    1L
  )
  expect_equal(
    substrait_compiler_function_id(compiler, "some_fun", list()),
    1L
  )
  expect_identical(
    as.list(compiler$function_extensions_key)[["1"]],
    list(name = "some_fun", arg_types = list())
  )

  # different arg types should trigger a new id
  expect_equal(
    substrait_compiler_function_id(
      compiler,
      "some_fun",
      list(
        substrait_i32()
      )
    ),
    2L
  )

  # ...but doing it again should get the same id
  expect_equal(
    substrait_compiler_function_id(
      compiler,
      "some_fun",
      list(
        substrait_i32()
      )
    ),
    2L
  )
})

test_that("substrait_compiler_function() works", {
  compiler <- substrait_compiler()

  expect_identical(
    substrait_compiler_function(
      compiler, "some_fun", list(1L),
      context = list(function_type = "scalar")
    ),
    substrait$Expression$create(
      scalar_function = substrait$Expression$ScalarFunction$create(
        function_reference = 1,
        args = list(
          substrait$Expression$create(
            literal = substrait$Expression$Literal$create(i32 = 1L)
          )
        ),
        output_type = substrait$Type$create()
      )
    )
  )

  expect_identical(
    substrait_compiler_function(
      compiler, "some_fun", list(1L),
      context = list(function_type = "window")
    ),
    substrait$Expression$create(
      window_function = substrait$Expression$WindowFunction$create(
        function_reference = 1,
        args = list(
          substrait$Expression$create(
            literal = substrait$Expression$Literal$create(i32 = 1L)
          )
        ),
        output_type = substrait$Type$create()
      )
    )
  )

  expect_identical(
    substrait_compiler_function(
      compiler, "some_fun", list(1L),
      context = list(function_type = "aggregate")
    ),
    substrait$AggregateFunction$create(
      function_reference = 1,
      args = list(
        substrait$Expression$create(
          literal = substrait$Expression$Literal$create(i32 = 1L)
        )
      ),
      output_type = substrait$Type$create()
    )
  )
})

test_that("substrait_compiler_next_id() works", {
  compiler <- substrait_compiler()
  expect_identical(substrait_compiler_next_id(compiler), 1L)
  expect_identical(substrait_compiler_next_id(compiler), 2L)
})


