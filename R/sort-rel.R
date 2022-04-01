
#' Append a Substrait Project Relation
#'
#' @inheritParams substrait_project
#' @param ... Expressions that evaluate to an Expression or SortField
#' @param expr An expression that evaluates to an Expression
#' @param direction A SortField.SortDirection
#'
#' @return A modified `.builder`
#' @export
#'
#' @examples
#' substrait_sort(
#'   data.frame(a = 1, b = "one"),
#'   a,
#'   substrait_sort_field(b, "SORT_DIRECTION_DESC_NULLS_LAST")
#' )
#'
substrait_sort <- function(.builder, ...) {
  .builder <- substrait_builder(.builder)

  quos <- rlang::enquos(...)

  context <- list(
    schema = .builder$schema,
    list_of_expressions = .builder$mask
  )

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
    compiler = .builder$compiler,
    context = context
  )

  rel <- substrait$Rel$create(
    sort = substrait$SortRel$create(
      input = .builder$plan$relations[[1]]$rel,
      sorts = sorts
    )
  )

  # update the builder
  .builder$plan$relations[[1]]$rel <- rel
  validate_substrait_builder(.builder)
  .builder
}

#' @rdname substrait_sort
#' @export
substrait_sort_field <- function(expr, direction = "SORT_DIRECTION_UNSPECIFIED") {
  expr <- as_substrait(expr, "substrait.Expression")
  substrait$SortField$create(expr = expr, direction = direction)
}

expr_replace_sort_field <- function(expr) {
  if (rlang::is_call(expr, "substrait_sort_field")) {
    expr[[1]] <- substrait_sort_field
    expr
  } else {
    expr
  }
}

# Take selected columns and create the appropriate substrait message
build_sort <- function(df, sort_cols, sort_desc) {
  # get numeric matches of column positions
  locs <- match(
    unname(vapply(sort_cols, as.character, character(1))),
    names(df)
  )

  # -1 as it's 0-indexed
  to_sort <- Map(list, field = locs - 1, desc = sort_desc)

  sort_expressions <- lapply(
    to_sort,
    sort_field
  )

  sort_expressions
}

sort_field <- function(ref) {
  substrait$SortField$create(
    expr = simple_integer_field_reference(ref$field),
    direction = dplyr_desc_to_substrait(ref$desc)
  )
}

# Convert from dplyr sort order to substrait sort order
dplyr_desc_to_substrait <- function(dplyr_desc) {
  if (dplyr_desc) {
    # SORT_DIRECTION_DESC_NULLS_LAST = 4
    4
  } else {
    # SORT_DIRECTION_ASC_NULLS_LAST = 2
    2
  }
}
