
#' Build a Substrait plan
#'
#' @param builder A [substrait_builder()]
#' @param consumer A [GenericConsumer] instance
#' @param rel A table-like object with which to create a builder.
#' @param ... Passed to the [GenericConsumer] when creating a new builder
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

#' @rdname substrait_builder
#' @export
substrait_evaluate <- function(builder, ...) {
  builder$consumer$evaluate_builder(builder, ...)
}

#' @export
print.substrait_builder <- function(x, ...) {
  try(x$consumer$print_builder(x, ...))
  invisible(x)
}

new_substrait_builder <- function(x) {
  structure(x, class = "substrait_builder")
}
