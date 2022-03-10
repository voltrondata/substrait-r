#' @export
substrait_schema <- function(x){
  UseMethod("substrait_schema", x)
}

#' Create a substrait schema
#' @param x data.frame object
#' @export
substrait_schema.data.frame <- function(x){
  as_substrait(x, "substrait.NamedStruct")
}


