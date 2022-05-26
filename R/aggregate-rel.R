
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

  quos <- rlang::enquos(..., .named = TRUE)
  context <- list(
    schema = .compiler$schema,
    list_of_expressions = .compiler$mask
  )

  # have to rethink this because we need to keep track of a
  # post_mutate step for expressions like sum(x) + 1
  measures <- lapply(
    quos,
    as_substrait,
    .ptype = "substrait.AggregateRel.Measure",
    compiler = .compiler,
    context = context,
    template = substrait$AggregateFunction$create()
  )

  .compiler$rel <- substrait$Rel$create(
    aggregate = substrait$AggregateRel$create(
      input = .compiler$rel,
      groupings = list(
        substrait$AggregateRel$Grouping$create(
          grouping_expressions = as.list(.compiler$groups)
        )
      ),
      measures = measures
    )
  )

  # reset mask and schema here (probably should do this in substrait_project
  # too)
  types <- c(
    lapply(
      .compiler$groups,
      as_substrait,
      .ptype = "substrait.Type",
      compiler = .compiler
    ),
    lapply(
      measures,
      as_substrait,
      .ptype = "substrait.Type",
      compiler = .compiler
    )
  )

  .compiler$schema <- substrait$NamedStruct$create(
    names = names(types),
    struct_ = substrait$Type$Struct$create(
      types = types
    )
  )

  .compiler$mask <- lapply(
    seq_along(types) - 1L,
    simple_integer_field_reference
  )
  names(.compiler$mask) <- names(types)

  # drop groups
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
