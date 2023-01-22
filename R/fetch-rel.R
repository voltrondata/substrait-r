
#' Append a Substrait Fetch Relation
#'
#' @param .compiler A [substrait_compiler()] or object that can be coerced to one
#' @param ... Expressions
#'
#' @return A modified `.compiler`
#' @export
#'
#' @examples
#' substrait_fetch(
#'   duckdb_substrait_compiler(data.frame(x = 1:10)),
#'   2:4
#' )
#'
substrait_fetch <- function(.compiler, ...) {
  .compiler <- substrait_compiler(.compiler)$clone()
  local_compiler(.compiler)

  rel <- substrait$Rel$create(
    fetch = substrait$FetchRel$create(
      input = .compiler$rel,
      offset = 2,
      count = 3
    )
  )

  # update the compiler
  .compiler$rel <- rel
  .compiler$validate()

}
