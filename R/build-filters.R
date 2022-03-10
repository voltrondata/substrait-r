#' Take filtered rows and create the appropriate substrait message
#'
#' @param filters list of quosures
#'
#' @export
build_filters <- function(filters, top_row){

  compiler <- substrait_compiler()
  context <- new_context(top_row)

  lapply(filters, as_substrait, .ptype = "substrait.Expression", compiler = compiler, context = context)

}
