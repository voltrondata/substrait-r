#' @export
build_plan <- function(x){
  UseMethod("build_plan", x)
}

#' @export
build_plan.substrait_dplyr_query <- function(x) {

  data <- as.data.frame(x)
  compiler <- substrait_compiler()

  filtered_rows <- attr(x, "filtered_rows")

  # do filtering before selection
  if (!is_empty(filtered_rows)) {
    plan <- substrait$FilterRel$create(
      # should this call to substrait$Rel$create go inside build_base_table??
      input = substrait$Rel$create(read = build_base_table(data)),
      condition = build_filters(data, filtered_rows, compiler)
    )
  }
  plan
}

build_base_table <- function(.data){

  .data <- as.data.frame(.data)

  # Currently using the known working example of reading in using a
  # parquet file
  temp_parquet_file <- tempfile()

  #arrow::write_parquet(.data, temp_parquet_file)

  input_table <- substrait$ReadRel$create(
    base_schema = as_substrait(.data, "substrait.NamedStruct"),
    local_files = substrait$ReadRel$LocalFiles$create(
      items = list(
        substrait$ReadRel$LocalFiles$FileOrFiles$create(
          uri_file = sprintf("file://%s", temp_parquet_file),
          format = substrait$ReadRel$LocalFiles$FileOrFiles$FileFormat$FILE_FORMAT_PARQUET
        )
      )
    )
  )

  input_table
}
