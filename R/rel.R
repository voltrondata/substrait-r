
#' Get column names for a relation
#'
#' @param x A Rel, PlanRel, or Plan
#'
#' @return A vector of column names
#' @export
#'
substrait_colnames <- function(x) {
  UseMethod("substrait_colnames")
}

#' @export
substrait_colnames.substrait_ReadRel <- function(x) {
  x$base_schema$names
}

#' @export
substrait_colnames.substrait_Rel <- function(x) {
  for (item in as.list(x)) {
    return(substrait_colnames(item))
  }

  return(NULL)
}

#' @export
substrait_colnames.substrait_PlanRel <- function(x) {
  substrait_colnames(x$rel)
}

#' @export
substrait_colnames.substrait_Plan <- function(x) {
  do.call(c, lapply(x$relations, substrait_colnames))
}

rel_tree_modify <- function(x, classes = character(), fun = identity) {
  if (inherits(x, classes)) {
    fun(x)
  } else if (inherits(x, "substrait_proto_message")) {
    x_items <- lapply(x, rel_tree_modify, classes, fun)
    substrait_create(make_qualified_name(x), !!! x_items)
  } else if (rlang::is_bare_list(x)) {
    lapply(x, rel_tree_modify, classes, fun)
  } else {
    x
  }
}
