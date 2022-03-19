
#' Arrange rows by columns values in a Substrait query
#'
#' @param .data A [substrait_dplyr_query()]
#' @inheritParams dplyr::arrange
#'
#' @return A [substrait_dplyr_query()]
#' @importFrom dplyr arrange
#' @export
#'
#' @examples
#' dplyr::arrange(
#'   substrait_dplyr_query(mtcars),
#'   hp, carb
#' )
arrange.substrait_dplyr_query <- function(.data, ..., .by_group = FALSE) {

  arrange_vars <- attr(.data, "arrange_vars")
  arrange_desc <- attr(.data, "arrange_desc")

  new_arrange_vars <- rlang::exprs(...)
  new_arrange_desc <- detect_desc(...)

  attr(.data, "arrange_vars") <- c(
    arrange_vars,
    new_arrange_vars
  )

  attr(.data, "arrange_desc") <- c(
    arrange_desc,
    new_arrange_desc
  )

  .data
}

# TODO: At a later point, copy arrow:::find_and_remove_desc here
detect_desc <- function(...){
  # For now, returns FALSE for each value in ...
  logical(length(rlang::exprs(...)))
}
