#' Create a substrait_dplyr_query object
#'
#' @param .data Either a data.frame, Arrow Table, substrait_dplyr_query object; whatever
#' @param selected_columns Columns to select
#' @param filtered_rows Rows to filter
#'
#' @export
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

base_table <- function(df) {
  substrait_dplyr_query(df, selected_columns = names(df))
}

#' @export
as.data.frame.substrait_dplyr_query <- function(x, ...){
  class(x) <- "data.frame"
  x
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
