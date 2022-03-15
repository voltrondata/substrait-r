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
    plan <- substrait$Rel$create(
      project = substrait$ProjectRel$create(
        input = plan,
        expressions = build_projections(data, selected_columns)
      )
    )
  }

  # Sort
  arrange_vars <- attr(x, "arrange_vars")
  arrange_desc <- attr(x, "arrange_desc")
  if (!rlang::is_empty(arrange_vars)) {
    plan <- substrait$Rel$create(
      sort = substrait$SortRel$create(
        input = plan,
        sorts = build_sort(data, arrange_vars, arrange_desc)
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

  input_table <- substrait$ReadRel$create(
    base_schema = as_substrait(.data, "substrait.NamedStruct"),
    named_table = substrait$ReadRel$NamedTable$create(
      names = "the_name_of_the_table"
    )
  )

  substrait$Rel$create(read = input_table)
}
