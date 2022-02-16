
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
  } else if (identical(.qualified_name, "substrait.Type")) {
    return(substrait$Type$create(fp64 = list()))
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
as_substrait.integer <- function(x, .ptype = NULL, ...) {
  if (is.null(.ptype)) {
    .ptype <- substrait$Expression$Literal$create(i32 = NaN)
  }

  .qualified_name <- make_qualified_name(.ptype)

  if (identical(.qualified_name, "substrait.Expression")) {
    return(
      substrait$Expression$create(
        literal = as_substrait.integer(x, "substrait.Expression.Literal")
      )
    )
  } else if (identical(.qualified_name, "substrait.Type")) {
    return(substrait$Type$create(i32 = list()))
  }

  if (length(x) == 1 && !("list" %in% names(.ptype))) {
    switch(
      .qualified_name,
      "substrait.Expression.Literal" = {
        if (is.na(x) && !is.nan(x)) {
          substrait$Expression$Literal$create(
            null = substrait$Type$create(i32 = list())
          )
        } else {
          substrait$Expression$Literal$create(i32 = x)
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
            lapply(x, as_substrait.integer, .ptype = "substrait.Expression.Literal")
          )
        )
      },
      NextMethod()
    )
  }
}

#' @export
as_substrait.logical <- function(x, .ptype = NULL, ...) {
  if (is.null(.ptype)) {
    .ptype <- substrait$Expression$Literal$create(boolean = TRUE)
  }

  .qualified_name <- make_qualified_name(.ptype)

  if (identical(.qualified_name, "substrait.Expression")) {
    return(
      substrait$Expression$create(
        literal = as_substrait.logical(x, "substrait.Expression.Literal")
      )
    )
  } else if (identical(.qualified_name, "substrait.Type")) {
    return(substrait$Type$create(bool_ = list()))
  }

  if (length(x) == 1 && !("list" %in% names(.ptype))) {
    switch(
      .qualified_name,
      "substrait.Expression.Literal" = {
        if (is.na(x) && !is.nan(x)) {
          substrait$Expression$Literal$create(
            null = substrait$Type$create(bool_ = list())
          )
        } else {
          substrait$Expression$Literal$create(boolean = x)
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
            lapply(x, as_substrait.logical, .ptype = "substrait.Expression.Literal")
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
      literal <- msg$literal
      if (is.null(literal)) {
        stop("Can't convert non-literal Expression to double()")
      }

      from_substrait(literal, x)
    },
    "substrait.Expression.Literal" = {
      lst <- as.list(msg)
      switch(
        names(lst)[1],
        "null" = NA_real_,
        "list" = {
          vapply(lst$list$values, from_substrait, double(1), double())
        },
        as.double(lst[[1]])
      )
    },
    NextMethod()
  )
}

#' @export
from_substrait.integer <- function(msg, x, ...) {
  .qualified_name <- make_qualified_name(msg)
  switch(
    .qualified_name,
    "substrait.Expression" = {
      literal <- msg$literal
      if (is.null(literal)) {
        stop("Can't convert non-literal Expression to integer()")
      }

      from_substrait(literal, x)
    },
    "substrait.Expression.Literal" = {
      lst <- as.list(msg)
      switch(
        names(lst)[1],
        "null" = NA_integer_,
        "list" = {
          vapply(lst$list$values, from_substrait, integer(1), integer())
        },
        as.integer(lst[[1]])
      )
    },
    NextMethod()
  )
}

#' @export
from_substrait.logical <- function(msg, x, ...) {
  .qualified_name <- make_qualified_name(msg)
  switch(
    .qualified_name,
    "substrait.Expression" = {
      literal <- msg$literal
      if (is.null(literal)) {
        stop("Can't convert non-literal Expression to logical()")
      }

      from_substrait(literal, x)
    },
    "substrait.Expression.Literal" = {
      lst <- as.list(msg)
      switch(
        names(lst)[1],
        "null" = NA,
        "list" = {
          vapply(lst$list$values, from_substrait, logical(1), logical())
        },
        as.logical(lst[[1]])
      )
    },
    NextMethod()
  )
}

