#' Select column
#'
#' @param .data substrait_Rel object
#' @inheritParams dplyr::select
#' @importFrom dplyr select
#' @export
select.substrait_dplyr_query <- function(.data, ...) {
  selected_columns <- attr(.data, "selected_columns")

  empty_df <- get_empty_df(selected_columns)

  # Named vector of column names/indices
  cols <- tidyselect::eval_select(rlang::expr(c(...)), empty_df)

  substrait_dplyr_query(
    .data,
    selected_columns = rlang::syms(rlang::set_names(selected_columns[cols], names(cols)))
  )

}
