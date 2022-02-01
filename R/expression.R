
#' @export
as_substrait.double <- function(x, .ptype = NULL, ...) {
  if (is.null(.ptype)) {
    .ptype <- substrait$Expression$Literal$create(fp64 = NaN)
  }

  .qualified_name <- make_qualified_name(.ptype)

  if (identical(.qualified_name, "substrait.Expression")) {
    return(
      substrait$Expression$create(
        literal = as_substrait.double(x, "substrait.Expression.Literal")
      )
    )
  }

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

#' @export
from_substrait.double <- function(msg, x, ...) {
  .qualified_name <- make_qualified_name(msg)
  switch(
    .qualified_name,
    "substrait.Expression" = {
      literal <- x$literal
      if (is.null(literal)) {
        stop("Can't convert non-literal Expression to double()")
      }

      from_substrait(literal, x)
    },
    "substrait.Expression.Literal" = {
      lst <- from_substrait(msg, list())
      switch(
        names(lst)[1],
        "null" = NA_real_,
        "list" = {
          # this should probably be fixed in from_substrait()
          lst2 <- lapply(lst$list$values, as_substrait)
          vapply(lst2, from_substrait, double(1), double())
        },
        as.double(lst[[1]])
      )
    },
    NextMethod()
  )
}
