
test_that("read_tpch_df() works", {
  for (table in tpch_tables()) {
    expect_s3_class(read_tpch_df(!!table), "tbl_df")
  }
})
