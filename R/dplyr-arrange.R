
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

  new_arrange_vars <- rlang::exprs(...)

  attr(.data, "arrange_vars") <- c(
    arrange_vars,
    new_arrange_vars
  )

  .data
}
