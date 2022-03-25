#' Create a Substrait query
#'
#' @param .data Either a data.frame, Arrow Table, substrait_dplyr_query object; whatever
#' @param selected_columns Columns to select
#' @param filtered_rows Rows to filter
#' @param arrange_vars Columns to sort on
#' @param arrange_desc Are sort columns in descending order?
#'
#' @return An object of class 'substrait_dplyr_query'
#' @export
#'
#' @examples
#' substrait_dplyr_query(mtcars)
substrait_dplyr_query <- function(.data,
                                  selected_columns = attr(.data, "selected_columns"),
                                  filtered_rows = attr(.data, "filtered_rows"),
                                  arrange_vars = attr(.data, "arrange_vars"),
                                  arrange_desc = attr(.data, "arrange_desc")) {
  selected_columns <- selected_columns %||%
    rlang::set_names(unlist(lapply(rlang::syms(names(.data)), rlang::quos)), names(.data))


  structure(
    .data,
    selected_columns = selected_columns,
    filtered_rows = filtered_rows,
    arrange_vars = arrange_vars,
    arrange_desc = arrange_desc,
    class = "substrait_dplyr_query"
  )
}

base_table <- function(.data) {
  substrait_dplyr_query(.data)
}

#' @export
as.data.frame.substrait_dplyr_query <- function(x, ...) {
  class(x) <- "data.frame"
  x
}
