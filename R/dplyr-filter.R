#' Filter a Substrait query
#'
#' @inheritParams select.substrait_dplyr_query
#' @inheritParams dplyr::filter
#'
#' @return A [substrait_dplyr_query()].
#'
#' @importFrom dplyr filter
#' @export
#'
#' @examples
#' dplyr::filter(
#'   substrait_dplyr_query(mtcars),
#'   hp > 100
#' )
#'
filter.substrait_dplyr_query <- function(.data, ..., .preserve = FALSE) {
  conditions <- rlang::quos(...)

  if (rlang::is_empty(conditions)) {
    return(.data)
  }

  existing_conditions <- attr(.data, "filtered_rows")

  updated_conditions <- append(
    # as.list in case it's NULL
    as.list(existing_conditions),
    conditions
  )

  substrait_dplyr_query(.data, filtered_rows = updated_conditions)
}
