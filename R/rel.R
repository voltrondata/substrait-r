
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
