
#' Select columns from a Substrait query
#'
#' @param .data A [substrait_dplyr_query()]
#' @inheritParams dplyr::select
#'
#' @return A [substrait_dplyr_query()]
#' @importFrom dplyr select
#' @export
#'
#' @examples
#' dplyr::select(
#'   substrait_dplyr_query(mtcars),
#'   hp, carb
#' )
#'
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

get_empty_df <- function(cols) {
  data.frame(
    matrix(
      ncol = length(cols),
      nrow = 0,
      dimnames = list(NULL, cols)
    )
  )
}
