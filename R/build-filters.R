
# Take filtered rows and create the appropriate substrait message
build_filters <- function(df, filters, compiler) {
  context <- new_context(df)
  lapply(
    filters,
    as_substrait,
    .ptype = "substrait.Expression",
    compiler = compiler,
    context = context
  )
}
