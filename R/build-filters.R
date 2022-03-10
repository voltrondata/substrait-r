#' Take filtered rows and create the appropriate substrait message
#'
#' @param filters list of quosures
#'
#' @export
build_filters <- function(filters, df, compiler){

  context <- new_context(df)
  lapply(filters, as_substrait, .ptype = "substrait.Expression", compiler = compiler, context = context)

}
