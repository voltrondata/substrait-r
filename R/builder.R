
#' Build a Substrait plan
#'
#' @param consumer A [Consumer] instance
#' @param rel A table-like object with which to create a builder.
#' @param ... Passed to the [Consumer] when creating a new builder
#'
#' @return An object of class 'substrait_builder'
#' @export
#'
#' @examples
#' substrait_builder(data.frame(col1 = 1 , col2 = "one"))
#'
substrait_builder <- function(rel, ..., consumer = GenericConsumer$new()) {
  UseMethod("substrait_builder")
}

#' @rdname substrait_builder
#' @export
substrait_builder.substrait_builder <- function(rel, ..., consumer = GenericConsumer$new()) {
  rel
}

#' @rdname substrait_builder
#' @export
substrait_builder.default <- function(rel, ..., consumer = GenericConsumer$new()) {
  consumer$create_builder(rel, ...)
}

new_substrait_builder <- function(x) {
  structure(x, class = "substrait_builder")
}
