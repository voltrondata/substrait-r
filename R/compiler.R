
#' Build a Substrait plan
#'
#' @param compiler A [substrait_compiler()]
#' @param compiler A [SubstraitCompiler] instance
#' @param rel A table-like object with which to create a compiler.
#' @param ... Passed to the [SubstraitCompiler] when creating a new compiler
#'
#' @return An object of class 'substrait_compiler'
#' @export
#'
#' @examples
#' substrait_compiler(data.frame(col1 = 1 , col2 = "one"))
#'
substrait_compiler <- function(rel, ..., compiler = SubstraitCompiler$new()) {
  UseMethod("substrait_compiler")
}

#' @rdname substrait_compiler
#' @export
substrait_compiler.SubstraitCompiler <- function(rel, ..., compiler = SubstraitCompiler$new()) {
  rel
}

#' @rdname substrait_compiler
#' @export
substrait_compiler.default <- function(rel, ..., compiler = SubstraitCompiler$new()) {
  compiler$create_compiler(rel, ...)
}
