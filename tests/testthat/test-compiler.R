
test_that("substrait_compiler() creates a compiler with a ReadRel from a data frame", {
  tbl <- data.frame(col1 = 1, col2 = "one")

  expect_s3_class(
    compiler <- substrait_compiler(tbl),
    "SubstraitCompiler"
  )

  expect_identical(
    compiler$schema,
    substrait$NamedStruct$create(
      names = c("col1", "col2"),
      struct_ = substrait$Type$Struct$create(
        types = list(
          substrait_fp64(),
          substrait_string()
        )
      )
    )
  )

  expect_identical(
    compiler$mask,
    list(
      col1 = simple_integer_field_reference(0L),
      col2 = simple_integer_field_reference(1L)
    )
  )

  expect_identical(compiler$groups, NULL)
  expect_s3_class(compiler$rel, "substrait_Rel")
  expect_identical(
    compiler$named_table(compiler$rel$read$named_table$names),
    tbl
  )
  expect_identical(
    compiler$named_table_list(),
    rlang::list2(!!(compiler$rel$read$named_table$names) := tbl)
  )
})

test_that("substrait_compiler() returns its input if it's already a compiler", {
  compiler <- substrait_compiler(data.frame(col1 = 1, col2 = "one"))
  expect_identical(substrait_compiler(compiler), compiler)
})


test_that("SubstraitCompiler can be created", {
  compiler <- SubstraitCompiler$new()
  expect_s3_class(compiler, "SubstraitCompiler")
})


test_that("SubstraitCompiler$next_id() works", {
  compiler <- SubstraitCompiler$new()
  expect_identical(compiler$next_id(), 1L)
  expect_identical(compiler$next_id(), 2L)
})

test_that("SubstraitCompiler$add_relation() works", {
  compiler <- SubstraitCompiler$new()

  # By default, objects are have their schemas extracted and are turned into
  # named tables
  tbl <- data.frame(a = 1L, b = "one")
  compiler <- compiler$add_relation(tbl)

  expect_s3_class(compiler$rel, "substrait_Rel")

  expect_match(compiler$rel$read$named_table$names, "^named_table_")
  expect_identical(
    compiler$named_table(compiler$rel$read$named_table$names),
    tbl
  )
})

test_that("SubstraitCompiler$new() takes an `object`", {
  tbl <- data.frame(a = 1L, b = "one")
  compiler <- SubstraitCompiler$new(tbl)
  expect_s3_class(compiler$rel, "substrait_Rel")
})

test_that("substrait_compiler_function_id() works", {
  compiler <- SubstraitCompiler$new()

  expect_equal(
    compiler$function_id("some_fun", list()),
    1L
  )
  expect_equal(
    compiler$function_id("some_fun", list()),
    1L
  )
  expect_identical(
    as.list(compiler$function_extension(1)),
    list(name = "some_fun", arg_types = list())
  )

  # different arg types should trigger a new id
  expect_equal(
    compiler$function_id(
      "some_fun",
      list(
        substrait_i32()
      )
    ),
    2L
  )

  # ...but doing it again should get the same id
  expect_equal(
    compiler$function_id(
      "some_fun",
      list(
        substrait_i32()
      )
    ),
    2L
  )
})

test_that("SubstraitCompiler$resolve_function() works", {
  compiler <- SubstraitCompiler$new()

  expect_identical(
    compiler$resolve_function(
      "some_fun",
      list(1L),
      substrait$Expression$ScalarFunction$create(),
      output_type = NULL
    ),
    substrait$Expression$ScalarFunction$create(
      function_reference = 1,
      arguments = list(
        substrait$FunctionArgument$create(
          value = substrait$Expression$create(
            literal = substrait$Expression$Literal$create(i32 = 1L)
          )
        )
      ),
      output_type = substrait$Type$create()
    )
  )

  expect_identical(
    compiler$resolve_function(
      "some_fun",
      list(1L),
      substrait$Expression$ScalarFunction$create(),
      output_type = substrait_boolean()
    ),
    substrait$Expression$ScalarFunction$create(
      function_reference = 1,
      arguments = list(
        substrait$FunctionArgument$create(
          value = substrait$Expression$create(
            literal = substrait$Expression$Literal$create(i32 = 1L)
          )
        )
      ),
      output_type = substrait_boolean()
    )
  )

  expect_identical(
    compiler$resolve_function(
      "some_fun",
      list(1L),
      substrait$Expression$ScalarFunction$create(),
      output_type = function(x) x
    ),
    substrait$Expression$ScalarFunction$create(
      function_reference = 1,
      arguments = list(
        substrait$FunctionArgument$create(
          value = substrait$Expression$create(
            literal = substrait$Expression$Literal$create(i32 = 1L)
          )
        )
      ),
      output_type = substrait_i32()
    )
  )
})

test_that("SubstraitCompiler$plan() includes rel and extensions", {
  df <- data.frame(col = 1L)

  compiler <- SubstraitCompiler$new(df)
  fun <- compiler$resolve_function(
    "some_fun",
    list(1L),
    substrait$Expression$ScalarFunction$create()
  )

  plan <- compiler$plan()
  expect_identical(
    plan$extension_uris[[1]]$extension_uri_anchor,
    1
  )

  plan <- compiler$plan()
  expect_identical(
    plan$extensions[[1]]$extension_function$function_anchor,
    fun$function_reference
  )
})

test_that("global substrait compiler value can be set and accessed", {
  expect_null(current_compiler())

  compiler1 <- substrait_compiler(data.frame())
  with_compiler(
    compiler1,
    expect_identical(current_compiler(), compiler1)
  )

  expect_null(current_compiler())

  local({
    compiler2 <- substrait_compiler(data.frame(a = 1))
    expect_identical(local_compiler(compiler2), compiler2)
    expect_identical(current_compiler(), compiler2)
  })

  expect_null(current_compiler())
})
