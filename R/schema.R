
#' Create a substrait schema
#'
#' @param x object to be turned into a schema
#' @export
#'
#' @examples
#' substrait_schema(data.frame(a = double(), b = integer()))
#'
substrait_schema <- function(x) {
  UseMethod("substrait_schema")
}

#' @rdname substrait_schema
#' @export
substrait_schema.substrait_dplyr_query <- function(x) {
  types <- lapply(
    x,
    as_substrait,
    "substrait.Type"
  )

  substrait$NamedStruct$create(
    names = names(x),
    struct_ = substrait$Type$Struct$create(
      types = types
    )
  )
}

#' @rdname substrait_schema
#' @export
substrait_schema.data.frame <- function(x) {
  as_substrait(x, "substrait.NamedStruct")
}
