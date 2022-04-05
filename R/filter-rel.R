
#' Append a Substrait Project Relation
#'
#' @param ... Filter expressions
#' @inheritParams substrait_project
#'
#' @return A modified `.builder`
#' @export
#'
#' @examples
#' substrait_filter(
#'   data.frame(a = 1, b = "one"),
#'   a > 0
#' )
#'
substrait_filter <- function(.builder, ...) {
  .builder <- substrait_compiler(.builder)
  .builder$consumer <- .builder$consumer$clone()

  quos <- rlang::enquos(...)
  if (length(quos) == 0) {
    quos <- rlang::quos(TRUE)
  }

  context <- list(
    schema = .builder$schema,
    list_of_expressions = .builder$mask
  )

  expressions <- lapply(
    quos,
    as_substrait,
    .ptype = "substrait.Expression",
    consumer = .builder$consumer,
    context = context
  )

  combined_expressions_quo <- Reduce("combine_expressions_and", expressions)
  combined_expressions <- as_substrait(
    combined_expressions_quo,
    "substrait.Expression"
  )

  rel <- substrait$Rel$create(
    filter = substrait$FilterRel$create(
      input = .builder$rel,
      condition = combined_expressions
    )
  )

  # update the builder
  .builder$rel <- rel
  .builder$consumer$validate_builder(.builder)
}


# Take filtered rows and create the appropriate substrait message
build_filters <- function(df, filters, consumer) {
  context <- new_context(df)

  expressions <- lapply(
    filters,
    as_substrait,
    .ptype = "substrait.Expression",
    consumer = consumer,
    context = context
  )

  combined_expressions <- Reduce("combine_expressions_and", expressions)
  as_substrait(combined_expressions, "substrait.Expression")
}

combine_expressions_and <- function(expr1, expr2) {
  rlang::quo(!!expr1 & !!expr2)
}
