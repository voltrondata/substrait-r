
#' @export
as_substrait.double <- function(x, .ptype = NULL, ...) {
  if (is.null(.ptype)) {
    .ptype <- substrait$Expression$Literal$create(fp64 = NaN)
  }

  .qualified_name <- make_qualified_name(.ptype)
  if (length(x) == 1 && !("list" %in% names(.ptype))) {
    switch(
      .qualified_name,
      "substrait.Expression.Literal" = {
        if (is.na(x) && !is.nan(x)) {
          substrait$Expression$Literal$create(
            null = substrait$Type$create(fp64 = list())
          )
        } else {
          substrait$Expression$Literal$create(fp64 = x)
        }
      },
      NextMethod()
    )
  } else {
    switch(
      .qualified_name,
      "substrait.Expression.Literal" = {
        substrait$Expression$Literal$create(
          list = substrait$Expression$Literal$List$create(
            lapply(x, as_substrait.double, .ptype = "substrait.Expression.Literal")
          )
        )
      },
      NextMethod()
    )
  }
}
