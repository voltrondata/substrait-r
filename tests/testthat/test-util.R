
test_that("read_tpch_df() works", {
  for (table in tpch_table_names()) {
    expect_s3_class(read_tpch_df(!!table), "tbl_df")
  }
})

test_that("tpch_tables() contains all tpch tables", {
  expect_named(tpch_tables(), tpch_table_names())
  for (table in tpch_table_names()) {
    expect_s3_class(tpch_tables()[[!!table]], "tbl_df")
  }
})
