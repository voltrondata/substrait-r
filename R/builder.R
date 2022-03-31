
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
substrait_builder <- function(tbl, ..., compiler = substrait_compiler()) {
  rel <- substrait_compiler_rel(compiler, tbl, ...)

  plan <- substrait$Plan$create(
    relations = list(
      substrait$PlanRel$create(
        rel = rel
      )
    )
  )

  new_substrait_builder(
    list(
      plan = plan,
      compiler = compiler,
      names = substrait_colnames(rel),
      groups = NULL
    )
  )
}

new_substrait_builder <- function(x) {
  structure(x, class = "substrait_builder")
}
