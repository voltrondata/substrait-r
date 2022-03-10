#' Select column
#'
#' @param .data substrait_Rel object
#' @inheritParams dplyr::select
#' @importFrom dplyr select
#' @export
select.substrait_dplyr_query <- function(.data, ...) {
  selected_columns <- attr(.data, "selected_columns")

  empty_df <- data.frame(
    matrix(
      ncol = length(selected_columns),
      nrow = 0,
      dimnames = list(NULL, selected_columns)
    )
  )

  # Named vector of column names/indices
  cols <- tidyselect::eval_select(rlang::expr(c(...)), empty_df)

  substrait_dplyr_query(
    .data,
    selected_columns = rlang::syms(rlang::set_names(selected_columns[cols], names(cols)))
  )

}

#' Create a substrait plan from
#'
#' @param x A substrait_dplyr_query object
#' @inheritParams as_substrait
#' @return A substrait plan
#'
#' @export
as_substrait.substrait_dplyr_query <- function(x, .ptype = NULL, ...) {
  # create the root relations
  rel <- substrait$PlanRel$create(
    root = substrait$RelRoot$create(
      input = base_table(x),
      names = names(x)
    )
  )
  substrait$Plan$create(relations = list(rel))
}

base_table <- function(df) {
  substrait_dplyr_query(df, selected_columns = names(df))
}
