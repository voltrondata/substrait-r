# this is adapted from arrowbench, as is the data

library(dplyr)
library(substrait)

input_func_duckdb_substrait <- function(name){
  file <- file.path("inst", "tpch", "tpch_data", paste0(name, "_0.1.parquet"))
  data <- arrow::read_parquet(file, as_data_frame = TRUE)
  return(duckdb_substrait_compiler(data))
}
source("inst/tpch/tpch_queries.R")

for (query in 1:22) {

  # retrieve results
  result_duckdb_via_substrait <- tpc_h_queries[[query]](input_func = input_func_duckdb_substrait)
  result_duckdb <- arrow::read_parquet(paste0("./inst/tpch/duckdb_answer_scale-factor-0.1/tpch-q", query, ".parquet"))

  print(
    waldo::compare(
      result_duckdb_via_substrait,
      result_duckdb,
      tolerance = 0.01, x_arg = "duckdb_sub", y_arg = "duckdb"
    )
  )

}



