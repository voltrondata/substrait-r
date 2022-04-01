
#' Build a Substrait plan
#'
#' @inheritParams substrait_compiler
#'
#' @return An object of class 'substrait_builder'
#' @export
#'
#' @examples
#' substrait_builder(data.frame(col1 = 1 , col2 = "one"))
#'
substrait_builder <- function(rel, ..., compiler = substrait_compiler()) {
  UseMethod("substrait_builder")
}

#' @rdname substrait_builder
#' @export
substrait_builder.substrait_builder <- function(rel, ..., compiler = substrait_compiler()) {
  # TODO: clone the compiler!
  rel
}

#' @rdname substrait_builder
#' @export
substrait_builder.default <- function(rel, ..., compiler = substrait_compiler()) {
  rel <- substrait_compiler_rel(compiler, rel, ...)

  plan <- substrait$Plan$create(
    relations = list(
      substrait$PlanRel$create(
        rel = rel
      )
    )
  )

  builder <- new_substrait_builder(
    list(
      plan = plan,
      compiler = compiler,
      schema = substrait_rel_schema(rel),
      mask = substrait_rel_mask(rel),
      groups = NULL
    )
  )

  validate_substrait_builder(builder)
  builder
}

validate_substrait_builder <- function(builder) {
  stopifnot(
    identical(builder$schema$names, names(builder$mask))
  )

  invisible(builder)
}


new_substrait_builder <- function(x) {
  structure(x, class = "substrait_builder")
}
