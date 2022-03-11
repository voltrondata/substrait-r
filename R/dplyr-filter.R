#' Filter column
#'
#' @param .data substrait_dplyr_query object or data.frame
#' @inheritParams dplyr::filter
#' @importFrom dplyr filter
#' @export
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

