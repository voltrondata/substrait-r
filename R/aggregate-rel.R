
#' Aggregate
#'
#' @inheritParams substrait_project
#' @param ...
#'   - `substrait_aggregate()`: A named list of expressions to be evaluated in the context
#'     of the aggregation.
#'   - `substrait_group_by()`: A list of expressions to be used as groupings
#'     for aggregation.
#'
#' @return A modified `.compiler`.
#' @export
#'
substrait_aggregate <- function(.compiler, ...) {
  .compiler <- substrait_compiler(.compiler)$clone()

  quos <- rlang::enquos(...)
  context <- list(
    schema = .compiler$schema,
    list_of_expressions = .compiler$mask
  )

  # have to rethink this because we might need to keep track of a
  # post_mutate step for aggregate functions that are
  measures <- lapply(
    quos,
    as_substrait,
    .ptype = "substrait.AggregateRel.Measure",
    compiler = .compiler,
    context = context,
    template = substrait$AggregateFunction$create()
  )

  grouping_names <- names(.compiler$groups)

  .compiler$rel <- substrait$AggregateRel$create(
    input = .compiler$rel,
    groupings = list(
      substrait$AggregateRel$Grouping$create(
        grouping_expressions = .compiler$groups
      )
    ),
    measures = measures
  )

  .compiler$groups <- NULL
  .compiler
}

#' @rdname substrait_aggregate
#' @export
substrait_group_by <- function(.compiler, ...) {
  .compiler <- substrait_compiler(.compiler)$clone()

  quos <- rlang::enquos(..., .named = TRUE)
  context <- list(
    schema = .compiler$schema,
    list_of_expressions = .compiler$mask
  )

  .compiler$groups <- lapply(
    quos,
    as_substrait,
    .ptype = "substrait.Expression",
    compiler = .compiler,
    context = context
  )

  .compiler
}

#' @rdname substrait_aggregate
#' @export
substrait_ungroup <- function(.compiler) {
  .compiler <- substrait_compiler(.compiler)$clone()
  .compiler$groups <- NULL
  .compiler
}
