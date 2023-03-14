
#' Aggregate
#'
#' @inheritParams substrait_select
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
  local_compiler(.compiler)

  quos <- rlang::enquos(..., .named = TRUE)

  # inner == sum(x) + 1 [i.e. one of the inner functions is an aggregate func]
  #   here, we have to first do the aggregation and then the projection
  # outer == sum(x+1) [i.e. the outermost function is an aggregate func]
  #   here, we have to first calculate the projection x+1, and then the sum
  # both == sum(x + sum(x + 1)) [i.e. an aggregate exists at both inner and outer levels]
  #   here, we want to do the projection x+1, then aggregate it, then project x + it, then sum that
  # note! this is not supported!

  ctx <- rlang::env(
    mask = .compiler,
    aggregations = list(),
    post_mutate = list()
  )
  # get the inner and outer aggregations
  for (i in seq_along(quos)) {
    # Iterate over the indices and not the names because names may be repeated
    # (which overwrites the previous name)
    name <- names(quos)[i]
    quosure <- quos[[i]]

    # get expr from quosure
    expr <- rlang::quo_get_expr(quosure)
    quo_env <- rlang::quo_get_env(quosure)

    # get all function calls in the expression
    funs_in_expr <- all_funs(expr)

    if (length(funs_in_expr) == 0) {
      # This branch only gets called at the top level, where expr is something
      # that is not a function call (could be a quosure, a symbol, or atomic
      # value). This needs to evaluate to a scalar or something that can be
      # converted to one.
      value <- as_substrait_expression(quosure, compiler = .compiler)

      # Scalars need to be added to post_mutate because they don't need
      # to be sent to the query engine as an aggregation
      ctx$post_mutate[[name]] <- value
    }

    # Start inspecting the expr to see what aggregations it involves
    # TODO: generate, don't hard-code these
    agg_funs <- c("sum", "mean", "max", "min", "n", "n_distinct")
    outer_agg <- funs_in_expr[1] %in% agg_funs
    inner_agg <- funs_in_expr[-1] %in% agg_funs

    # First, pull out any aggregations wrapped in other function calls
    if (any(inner_agg)) {
      expr <- extract_aggregations(expr, ctx, agg_funs)
    }

    # By this point, there are no more aggregation functions in expr
    # except for possibly the outer function call:
    # they've all been pulled out to ctx$aggregations, and in their place in expr
    # there are variable names, which would correspond to field refs in the
    # query object after aggregation and collapse() or non-field variable
    # references. So if we want to know if there are any aggregations inside expr,
    # we have to look for them by their new var names in ctx$aggregations.
    inner_agg_exprs <- all_vars(expr) %in% names(ctx$aggregations)
    inner_is_fieldref <- all_vars(expr) %in% names(ctx$mask$.data)

    if (outer_agg) {
      # This is something like agg(fun(x, y)
      # It just works by normal arrow_eval, unless there's a mix of aggs and
      # columns in the original data like agg(fun(x, agg(x)))
      # (but that will have been caught in extract_aggregations())
      ctx$aggregations[[name]] <- rlang::as_quosure(expr, env = ctx$quo_env)
    } else if (all(inner_agg_exprs | !inner_is_fieldref)) {
      # Something like: fun(agg(x), agg(y))
      ctx$post_mutate[[name]] <- rlang::as_quosure(expr, env = ctx$quo_env)
    }
  }

  measures <- lapply(
    ctx$aggregations,
    as_substrait,
    .ptype = "substrait.AggregateRel.Measure",
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

  # reset mask and schema here
  types <- c(
    lapply(
      .compiler$groups,
      as_substrait_type
    ),
    lapply(
      measures,
      as_substrait_type
    )
  )

  .compiler$schema <- substrait$NamedStruct$create(
    names = names(types),
    struct = substrait$Type$Struct$create(
      types = types
    )
  )

  .compiler$.data <- lapply(
    seq_along(types) - 1L,
    simple_integer_field_reference
  )
  names(.compiler$.data) <- names(types)

  # drop groups
  grps <- .compiler$groups
  .compiler$groups <- NULL

  if (length(ctx$post_mutate) > 0) {
    # add in post-mutate cols
    vars_to_select <- c(names(grps), names(quos))

    .compiler <- substrait_project(.compiler, !!!syms(.compiler$schema$names), !!!ctx$post_mutate)

    .compiler <- substrait_select(.compiler, !!!syms(vars_to_select))
  }

  .compiler
}

#' @rdname substrait_aggregate
#' @export
substrait_group_by <- function(.compiler, ...) {
  .compiler <- substrait_compiler(.compiler)$clone()
  local_compiler(.compiler)

  quos <- rlang::enquos(..., .named = TRUE)
  if (length(quos) == 0) {
    .compiler$groups <- NULL
    return(.compiler)
  } else {
    # add any new groups to the data
    .compiler <- dplyr::mutate(.compiler, !!!quos)
  }

  .compiler$groups <- lapply(
    quos,
    as_substrait,
    .ptype = "substrait.Expression"
  )

  .compiler
}

#' @rdname substrait_aggregate
#' @export
substrait_ungroup <- function(.compiler) {
  substrait_group_by(.compiler)
}

#' @export
as_substrait.substrait_AggregateFunction <- function(x, .ptype = NULL, ...) {
  if (is.null(.ptype)) {
    .ptype <- x
  }

  switch(make_qualified_name(.ptype),
    "substrait.Expression" = {
      current_compiler()$post_mutate(x)
    },
    NextMethod()
  )
}

# This function recurses through expr, pulls out any aggregation expressions,
# and inserts a variable name (field ref) in place of the aggregation
extract_aggregations <- function(expr, ctx, agg_funcs) {
  # Keep the input in case we need to raise an error message with it
  original_expr <- expr
  funs <- all_funs(expr)
  if (length(funs) == 0) {
    return(expr)
  } else if (length(funs) > 1) {
    # Recurse more
    expr[-1] <- lapply(expr[-1], extract_aggregations, ctx, agg_funcs)
  }
  if (funs[1] %in% agg_funcs) {
    inner_agg_exprs <- all_vars(expr) %in% names(ctx$aggregations)
    if (any(inner_agg_exprs)) {
      # We can't aggregate over a combination of dataset columns and other
      # aggregations (e.g. sum(x - mean(x)))
      abort(
        paste(
          "Aggregate within aggregate expression not supported"
        )
      )
    }

    # We have an aggregation expression with no other aggregations inside it,
    # so arrow_eval the expression on the data and give it a ..temp name prefix,
    # then insert that name (symbol) back into the expression so that we can
    # mutate() on the result of the aggregation and reference this field.
    tmpname <- paste0("..temp", length(ctx$aggregations))
    ctx$aggregations[[tmpname]] <- rlang::as_quosure(expr, env = ctx$quo_env)
    expr <- as.symbol(tmpname)
  }
  expr
}
