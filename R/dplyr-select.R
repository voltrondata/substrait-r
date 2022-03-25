
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
select.substrait_dplyr_query <- function(.data, ...) {
  #browser()
  selected_columns <- attr(.data, "selected_columns")

  empty_df <- get_empty_df(names(selected_columns))

  # Vector of selected column names
  new_selected_columns <- names(tidyselect::eval_select(rlang::expr(c(...)), empty_df))

  # Hacky - only works as it'll choose the
  possible_columns <- append(rlang::quos(...), selected_columns[new_selected_columns])

  substrait_dplyr_query(
    .data,
    selected_columns = possible_columns[new_selected_columns]
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
