
#' dplyr verb implementations
#'
#' @param .data A [substrait_builder()]
#' @param ...
#'   - `select()`: see [dplyr::select()]
#'   - `filter()`: see [dplyr::filter()]
#'   - `mutate()`: see [dplyr::mutate()]
#'   - `arrange()`: see [dplyr::arrange()]
#'
#' @return A modified [substrait_builder()]
#' @export
#'
select.substrait_builder <- function(.data, ...) {
  .data <- substrait_builder(.data)

  # Named vector of column names/indices
  column_indices <- tidyselect::eval_select(
    rlang::expr(c(...)),
    from_substrait(.data$schema, data.frame())
  )

  new_mask <- rlang::set_names(.data$mask[column_indices], names(column_indices))
  substrait_project(.data, !!! new_mask)
}

#' @rdname select.substrait_builder
#' @export
filter.substrait_builder <- function(.data, ...) {
  substrait_filter(.data, ...)
}

simulate_data_frame <- function(builder) {
  from_substrait(builder$schema, data.frame())
}
