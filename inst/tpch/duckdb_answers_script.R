library(substrait)
library(dplyr)
library(duckdb)
library(arrowbench)


sf <- 0.1
tpch_files <- ensure_source("tpch", scale_factor = sf)

con <- dbConnect(duckdb::duckdb("temp"))
dbExecute(con, paste0("PRAGMA threads=10"))

# DuckDB tables
for (name in tpch_tables) {
  file <- path.expand(tpch_files[[name]])

  sql_query <- paste0("CREATE TABLE ", name, " AS SELECT * FROM parquet_scan('", file, "');")

  file <- tpch_files[[name]]
  dbExecute(con, sql_query)
}

input_functions[["duckdb"]] <- function(name) {
  return(dplyr::tbl(con, name))
}

for (q in c(1:22)) {
  message("==================================================")
  message(glue::glue("Query: {q}"))
  message("==================================================")

  tryCatch({

    query <- q

    # grab the sql queries from github (this URL might need to be updated if their location in the repo changes.)
    sql <- paste0(httr::GET(
      glue::glue("https://raw.githubusercontent.com/duckdb/duckdb/master/extension/tpch/dbgen/queries/q{stringr::str_pad(query, 2, pad = '0')}.sql")
    ), collapse = "\n")

    result_duckdb <- as_tibble(dbGetQuery(con, sql))

    arrow::write_parquet(result_duckdb, file.path("answers", "scale-factor-0.1", paste0("tpch-q", q, ".parquet")))

 }, error = function(e) print(paste("Error in query", q, ":", conditionMessage(e))))
}
