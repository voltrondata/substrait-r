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

get_empty_df <- function(cols){
    data.frame(
      matrix(
        ncol = length(cols),
        nrow = 0,
        dimnames = list(NULL, cols)
      )
    )
  }
