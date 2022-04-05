
#' Build a Substrait plan
#'
#' @param builder A [substrait_compiler()]
#' @param consumer A [SubstraitCompiler] instance
#' @param rel A table-like object with which to create a builder.
#' @param ... Passed to the [SubstraitCompiler] when creating a new builder
#'
#' @return An object of class 'substrait_compiler'
#' @export
#'
#' @examples
#' substrait_compiler(data.frame(col1 = 1 , col2 = "one"))
#'
substrait_compiler <- function(rel, ..., consumer = SubstraitCompiler$new()) {
  UseMethod("substrait_compiler")
}

#' @rdname substrait_compiler
#' @export
substrait_compiler.substrait_compiler <- function(rel, ..., consumer = SubstraitCompiler$new()) {
  rel
}

#' @rdname substrait_compiler
#' @export
substrait_compiler.default <- function(rel, ..., consumer = SubstraitCompiler$new()) {
  consumer$create_builder(rel, ...)
}

new_substrait_compiler <- function(x) {
  structure(x, class = "substrait_compiler")
}
