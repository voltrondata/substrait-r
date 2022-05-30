
#' Append a Substrait Project Relation
#'
#' @param ... Filter expressions
#' @inheritParams substrait_project
#'
#' @return A modified `.compiler`
#' @export
#'
#' @examples
#' substrait_filter(
#'   data.frame(a = 1, b = "one"),
#'   a > 0
#' )
#'
substrait_filter <- function(.compiler, ...) {
  .compiler <- substrait_compiler(.compiler)$clone()

  quos <- rlang::enquos(...)
  if (length(quos) == 0) {
    quos <- rlang::quos(TRUE)
  }

  expressions <- lapply(
    quos,
    as_substrait,
    .ptype = "substrait.Expression",
    compiler = .compiler
  )

  combined_expressions_quo <- Reduce("combine_expressions_and", expressions)
  combined_expressions <- as_substrait(
    combined_expressions_quo,
    "substrait.Expression"
  )

  rel <- substrait$Rel$create(
    filter = substrait$FilterRel$create(
      input = .compiler$rel,
      condition = combined_expressions
    )
  )

  # update the compiler
  .compiler$rel <- rel
  .compiler$validate()
}


# Take filtered rows and create the appropriate substrait message
build_filters <- function(compiler, filters) {
  expressions <- lapply(
    filters,
    as_substrait,
    .ptype = "substrait.Expression",
    compiler = compiler
  )

  combined_expressions <- Reduce("combine_expressions_and", expressions)
  as_substrait(combined_expressions, "substrait.Expression")
}

combine_expressions_and <- function(expr1, expr2) {
  rlang::quo(!!expr1 & !!expr2)
}
