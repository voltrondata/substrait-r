
#' @export
as_substrait.double <- function(x, .ptype = NULL, ...) {
  .qualified_name <- .qualified_name %||% "substrait.Expression.Literal.Decimal"
  switch(
    .qualified_name,
    "substrait.Expression.Literal.Decimal" =
      substrait$Expression$Literal$Decimal$create(x),
    "substrait.Expression.Literal" =
      substrait$Expression$Literal
  )
}

