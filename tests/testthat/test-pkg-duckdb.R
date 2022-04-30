
test_that("duckdb can roundtrip a substrait plan", {
  skip_if_not(has_duckdb_with_substrait())

  plan <- duckdb_get_substrait(
    "SELECT * from mtcars",
    tables = list(mtcars = mtcars)
  )

  # not sure why the table name doesn't come through here
  plan <- rel_tree_modify(plan, "substrait_ReadRel_NamedTable", function(x) {
    x$names <- "mtcars"
    x
  })

  expect_identical(
    duckdb_from_substrait(plan, tables = list(mtcars = mtcars)),
    tibble::as_tibble(mtcars)
  )
})

test_that("blob encoder works", {
  expect_identical(
    duckdb_encode_blob(as.raw(1:5)),
    "'\\x01\\x02\\x03\\x04\\x05'::BLOB"
  )

  skip_if_not(has_duckdb_with_substrait())
  tbl <- query_duckdb_with_substrait(
    paste0("SELECT ", duckdb_encode_blob(as.raw(1:5)), " as col")
  )
  expect_identical(tbl$col[[1]], as.raw(1:5))
})
