#' Take filtered rows and create the appropriate substrait message
#'
#' @param df data
#' @param filters list of quosures
#' @param compiler substrait compiler
#'
#' @export
build_filters <- function(df, filters, compiler){

  context <- new_context(df)
  lapply(filters, as_substrait, .ptype = "substrait.Expression", compiler = compiler, context = context)

}
