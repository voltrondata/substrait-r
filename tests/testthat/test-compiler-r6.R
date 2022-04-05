
test_that("SubstraitCompiler can be created", {
  consumer <- SubstraitCompiler$new()
  expect_s3_class(consumer, "SubstraitCompiler")
})


test_that("SubstraitCompiler$next_id() works", {
  consumer <- SubstraitCompiler$new()
  expect_identical(consumer$next_id(), 1L)
  expect_identical(consumer$next_id(), 2L)
})

test_that("SubstraitCompiler$create_builder() works", {
  consumer <- SubstraitCompiler$new()

  # By default, objects are have their schemas extracted and are turned into
  # named tables
  tbl <- data.frame(a = 1L, b = "one")
  builder <- consumer$create_builder(tbl)

  expect_s3_class(builder$rel, "substrait_Rel")

  expect_match(builder$rel$read$named_table$names, "^named_table_")
  expect_identical(
    consumer$named_table(builder$rel$read$named_table$names),
    tbl
  )
})

test_that("substrait_compiler_function_id() works", {
  consumer <- SubstraitCompiler$new()

  expect_equal(
    consumer$function_id("some_fun", list()),
    1L
  )
  expect_equal(
    consumer$function_id("some_fun", list()),
    1L
  )
  expect_identical(
    as.list(consumer$function_extension(1)),
    list(name = "some_fun", arg_types = list())
  )

  # different arg types should trigger a new id
  expect_equal(
    consumer$function_id(
      "some_fun",
      list(
        substrait_i32()
      )
    ),
    2L
  )

  # ...but doing it again should get the same id
  expect_equal(
    consumer$function_id(
      "some_fun",
      list(
        substrait_i32()
      )
    ),
    2L
  )
})


test_that("SubstraitCompiler$resolve_function() works", {
  consumer <- SubstraitCompiler$new()

  expect_identical(
    consumer$resolve_function(
      "some_fun",
      list(1L),
      substrait$Expression$ScalarFunction$create()
    ),
    substrait$Expression$ScalarFunction$create(
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
