
# Take filtered rows and create the appropriate substrait message
build_filters <- function(df, filters, compiler) {
  context <- new_context(df)

  expressions <- lapply(
    filters,
    as_substrait,
    .ptype = "substrait.Expression",
    compiler = compiler,
    context = context
  )

  combined_expressions <- Reduce("combine_expressions_and", expressions)
  as_substrait(combined_expressions, "substrait.Expression")
}

combine_expressions_and <- function(expr1, expr2) {
  rlang::quo(!!expr1 & !!expr2)
}
