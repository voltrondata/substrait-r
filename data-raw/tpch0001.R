
library(duckdb)

drv <- duckdb::duckdb()
con <- DBI::dbConnect(drv)
DBI::dbExecute(con, "INSTALL tpch; LOAD tpch;")
DBI::dbExecute(con, "CALL dbgen(sf=0.001)")

tpch_tables <- c("customer", "lineitem", "nation", "orders", "part", "partsupp", "region", "supplier")

for (table in tpch_tables) {
  file <- glue::glue("inst/extdata/tpch0001/{table}.csv")
  sql <- glue::glue("SELECT * from {table}")

  res <- DBI::dbSendQuery(con, sql, arrow = TRUE)
  reader <- duckdb::duckdb_fetch_record_batch(res)
  table <- reader$read_table()
  arrow::write_csv_arrow(table, file)
}
