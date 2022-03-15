#' Build Substrait Plan
#'
#' @param x Object used to build plan
#' @export
build_plan <- function(x) {
  UseMethod("build_plan", x)
}


#' @export
build_plan.substrait_dplyr_query <- function(x) {
  data <- as.data.frame(x)
  compiler <- substrait_compiler()

  filtered_rows <- attr(x, "filtered_rows")

  plan <- build_base_table(data)

  # do filtering before selection
  if (!rlang::is_empty(filtered_rows)) {
    plan <- substrait$Rel$create(
      filter = substrait$FilterRel$create(
        input = plan,
        condition = build_filters(data, filtered_rows, compiler)
      )
    )
  }

  # Projection/Selection
  selected_columns <- attr(x, "selected_columns")
  if (!rlang::is_empty(selected_columns) && !identical(names(selected_columns), names(x))) {
    plan <- substrait$ProjectRel$create(
      input = plan,
      expressions = build_projections(data, selected_columns)
    )
  }

  plan
}

build_base_table <- function(.data) {
  .data <- as.data.frame(.data)

  # Currently using the known working example of reading in using a
  # parquet file
  temp_parquet_file <- tempfile()

  # arrow::write_parquet(.data, temp_parquet_file)

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

  substrait$Rel$create(read = input_table)
}
