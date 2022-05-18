
#' Append a Substrait Project Relation
#'
#' @inheritParams substrait_project
#' @param ... Expressions that evaluate to an Expression or SortField
#' @param expr An expression that evaluates to an Expression
#' @param direction A SortField.SortDirection
#'
#' @return A modified `.compiler`
#' @export
#'
#' @examples
#' substrait_sort(
#'   data.frame(a = 1, b = "one"),
#'   a,
#'   substrait_sort_field(b, "SORT_DIRECTION_DESC_NULLS_LAST")
#' )
#'
substrait_sort <- function(.compiler, ...) {
  .compiler <- substrait_compiler(.compiler)$clone()

  quos <- rlang::enquos(...)

  context <- list(
    schema = .compiler$schema,
    list_of_expressions = .compiler$mask
  )

  # Rather than an expression, the SortRel needs a list of SortFields,
  # each of which is an Expression + a sort direction. Rather than use
  # desc(), which is specific to dplyr and only lets us specify one
  # sort direction, we define our own wrapper substrait_sort_field()
  # to allow marking an expression with a sort direction. We don't want
  # substrait_sort_field() to get translated by the SubstraitCompiler,
  # so we have to inline references to it in the user-provided expressions.
  with_inlined_sort_field <- lapply(
    quos,
    function(quo) {
      rlang::quo_set_expr(
        quo,
        expr_replace_sort_field(rlang::quo_get_expr(quo))
      )
    }
  )

  sorts <- lapply(
    with_inlined_sort_field,
    as_substrait,
    .ptype = "substrait.SortField",
    compiler = .compiler,
    context = context
  )

  rel <- substrait$Rel$create(
    sort = substrait$SortRel$create(
      input = .compiler$rel,
      sorts = sorts
    )
  )

  # update the compiler
  .compiler$rel <- rel
  .compiler$validate()
}

#' @rdname substrait_sort
#' @export
substrait_sort_field <- function(expr, direction = "SORT_DIRECTION_ASC_NULLS_LAST") {
  expr <- as_substrait(expr, "substrait.Expression")
  substrait$SortField$create(expr = expr, direction = direction)
}

expr_replace_sort_field <- function(expr) {
  # if the outer call in expr is substrait_sort_field(...), inline the
  # function so that the symbol isn't translated by the SubstraitCompiler
  if (rlang::is_call(expr, "substrait_sort_field")) {
    expr[[1]] <- substrait_sort_field
    expr
  } else {
    expr
  }
}
