
#' Append a Substrait Fetch Relation
#'
#' @param .compiler A [substrait_compiler()] or object that can be coerced to one
#' @param offset Number of rows to skip before returning rows
#' @param count Number of rows to return
#'
#' @return A modified `.compiler`
#' @export
#'
#' @examplesIf has_duckdb_with_substrait()
#' substrait_fetch(
#'   duckdb_substrait_compiler(data.frame(x = 1:10)),
#'   offset = 2,
#'   count = 2
#' )
#'
substrait_fetch <- function(.compiler, offset = 0, count) {
  .compiler <- substrait_compiler(.compiler)$clone()
  local_compiler(.compiler)

  rel <- substrait$Rel$create(
    fetch = substrait$FetchRel$create(
      input = .compiler$rel,
      offset = offset,
      count = count
    )
  )

  # update the compiler
  .compiler$rel <- rel
  .compiler$validate()

}
