#' Create a substrait_dplyr_query object
#'
#' @param .data Either a data.frame, Arrow Table, substrait_dplyr_query object; whatever
#' @param selected_columns Columns to select
#' @param filtered_rows Rows to filter
#'
#' This will need refining later but it's a useful convenience function for now
#' as it can operate on whatever
substrait_dplyr_query <- function(.data,
                                  selected_columns = attr(.data, "selected_columns"),
                                  filtered_rows = attr(.data, "filtered_rows")){
  structure(
    .data,
    selected_columns = selected_columns,
    filtered_rows = filtered_rows,
    class = "substrait_dplyr_query"
  )
}

#' @export
as.data.frame.substrait_dplyr_query <- function(.data){
  class(.data) <- "data.frame"
  .data
}

build_base_table <- function(.data){

  .data <- as.data.frame(.data)

  input_table <- substrait$ReadRel$create(
    base_schema = as_substrait(.data, "substrait.NamedStruct"),
    named_table = substrait$ReadRel$NamedTable$create(names = "the_name_of_the_table")
  )

  input_table
}

build_plan <- function(.data){

  plan_contents <- substrait$Rel$create(read = build_base_table(.data))

  if (!is.null(attr(.data, "filtered_rows"))) {

    plan <- substrait$FilterRel$create(
      input = input,
      condition = build_filters(attr(.data, "filtered_rows"))
    )

  }

  if (!is.null(attr(.data, "selected_columns"))) {

    plan <- substrait$ProjectRel$create(
      input = plan,
      expressions = build_projections(attr(.data, "selected_columns"), names(.data))
    )

  }

  plan <- substrait$Plan$create(
    relations = list(
      substrait$PlanRel$create(
        rel =
      )
    )
  )

  plan
}

get_empty_df <- function(cols){
    data.frame(
      matrix(
        ncol = length(cols),
        nrow = 0,
        dimnames = list(NULL, cols)
      )
    )
  }
