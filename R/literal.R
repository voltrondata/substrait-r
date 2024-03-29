
#' @export
as_substrait.substrait_Expression_Literal <- function(x, .ptype = NULL, ...) {
  if (is.null(.ptype)) {
    .ptype <- x
  }

  switch(make_qualified_name(.ptype),
    "substrait.Expression" = substrait$Expression$create(literal = x),
    "substrait.Type" = {
      which_literal <- names(x)
      if (length(which_literal) == 0) {
        return(substrait$Type$create())
      }

      # a few of these have to be renamed because the field names are
      # inconsistent between substrait.Type and substrait.Expression.Literal
      guessed_type <- switch(which_literal,
        boolean = "bool",
        which_literal
      )

      requested_type <- names(.ptype)
      if (length(requested_type) > 0) {
        stopifnot(identical(requested_type, guessed_type))
      }

      rlang::exec(
        substrait$Type$create,
        !!guessed_type := substrait_proto_auto(nullability = "NULLABILITY_NULLABLE")
      )
    },
    NextMethod()
  )
}

#' @export
as_substrait.data.frame <- function(x, .ptype = NULL, ...) {
  if (is.null(.ptype)) {
    stop("Can't guess default .ptype for as_substrait(<data.frame>)")
  }

  .qualified_name <- make_qualified_name(.ptype)
  switch(.qualified_name,
    "substrait.NamedStruct" = {
      types <- lapply(x, as_substrait_type)
      substrait$NamedStruct$create(
        names = names(x),
        struct = substrait$Type$Struct$create(
          types = types
        )
      )
    },
    NextMethod()
  )
}

#' @export
from_substrait.data.frame <- function(msg, x, ...) {
  .qualified_name <- make_qualified_name(msg)

  switch(.qualified_name,
    "substrait.NamedStruct" = {
      if (length(x) == 0) {
        ptype <- rep_len(list(vctrs::unspecified()), length(msg$names))
        names(ptype) <- msg$names
        class(ptype) <- class(x)
      } else {
        ptype <- x
      }

      stopifnot(identical(names(ptype), msg$names))
      ptype <- Map(from_substrait, msg$struct$types, ptype)
      names(ptype) <- msg$names
      x[names(ptype)] <- ptype
      x
    },
    NextMethod()
  )
}

#' @export
from_substrait.vctrs_unspecified <- function(msg, x, ...) {
  .qualified_name <- make_qualified_name(msg)

  switch(.qualified_name,
    "substrait.Type" = {
      type <- names(msg)
      if (length(type) == 0) {
        return(vctrs::unspecified())
      }

      switch(type,
        "bool" = logical(),
        "i32" = integer(),
        "fp64" = double(),
        "string" = character(),
        "date" = structure(numeric(0), class = "Date"),
        stop(sprintf("Can't convert substrait.Type<%s> to ptype<any>", type))
      )
    },
    NextMethod()
  )
}

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
    return(substrait_fp64())
  }

  if (length(x) == 1 && !("list" %in% names(.ptype))) {
    switch(.qualified_name,
      "substrait.Expression.Literal" = {
        if (is.na(x) && !is.nan(x)) {
          substrait$Expression$Literal$create(
            null = substrait_fp64()
          )
        } else {
          substrait$Expression$Literal$create(fp64 = x)
        }
      },
      NextMethod()
    )
  } else {
    switch(.qualified_name,
      "substrait.Expression.Literal" = {
        substrait$Expression$Literal$create(
          list = substrait$Expression$Literal$List$create(
            values = lapply(x, as_substrait.double, .ptype = "substrait.Expression.Literal")
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
    return(substrait_i32())
  }

  if (length(x) == 1 && !("list" %in% names(.ptype))) {
    switch(.qualified_name,
      "substrait.Expression.Literal" = {
        if (is.na(x) && !is.nan(x)) {
          substrait$Expression$Literal$create(
            null = substrait_i32()
          )
        } else {
          substrait$Expression$Literal$create(i32 = x)
        }
      },
      NextMethod()
    )
  } else {
    switch(.qualified_name,
      "substrait.Expression.Literal" = {
        substrait$Expression$Literal$create(
          list = substrait$Expression$Literal$List$create(
            values = lapply(x, as_substrait.integer, .ptype = "substrait.Expression.Literal")
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
    return(substrait_boolean())
  }

  if (length(x) == 1 && !("list" %in% names(.ptype))) {
    switch(.qualified_name,
      "substrait.Expression.Literal" = {
        if (is.na(x) && !is.nan(x)) {
          substrait$Expression$Literal$create(
            null = substrait_boolean()
          )
        } else {
          substrait$Expression$Literal$create(boolean = x)
        }
      },
      NextMethod()
    )
  } else {
    switch(.qualified_name,
      "substrait.Expression.Literal" = {
        substrait$Expression$Literal$create(
          list = substrait$Expression$Literal$List$create(
            values = lapply(x, as_substrait.logical, .ptype = "substrait.Expression.Literal")
          )
        )
      },
      NextMethod()
    )
  }
}

#' @export
as_substrait.character <- function(x, .ptype = NULL, ...) {
  if (is.null(.ptype)) {
    .ptype <- substrait$Expression$Literal$create(string = "")
  }

  .qualified_name <- make_qualified_name(.ptype)

  if (identical(.qualified_name, "substrait.Expression")) {
    return(
      substrait$Expression$create(
        literal = as_substrait.character(x, "substrait.Expression.Literal")
      )
    )
  } else if (identical(.qualified_name, "substrait.Type")) {
    return(substrait_string())
  }

  if (length(x) == 1 && !("list" %in% names(.ptype))) {
    switch(.qualified_name,
      "substrait.Expression.Literal" = {
        if (is.na(x) && !is.nan(x)) {
          substrait$Expression$Literal$create(
            null = substrait_string()
          )
        } else {
          substrait$Expression$Literal$create(string = x)
        }
      },
      NextMethod()
    )
  } else {
    switch(.qualified_name,
      "substrait.Expression.Literal" = {
        substrait$Expression$Literal$create(
          list = substrait$Expression$Literal$List$create(
            values = lapply(x, as_substrait.character, .ptype = "substrait.Expression.Literal")
          )
        )
      },
      NextMethod()
    )
  }
}

#' @export
as_substrait.Date <- function(x, .ptype = NULL, ...) {
  if (is.null(.ptype)) {
    .ptype <- substrait$Expression$Literal$create(date = NaN)
  }

  .qualified_name <- make_qualified_name(.ptype)

  if (identical(.qualified_name, "substrait.Expression")) {
    return(
      substrait$Expression$create(
        literal = as_substrait.Date(x, "substrait.Expression.Literal")
      )
    )
  } else if (identical(.qualified_name, "substrait.Type")) {
    return(substrait_date())
  }

  if (length(x) == 1 && !("list" %in% names(.ptype))) {
    switch(.qualified_name,
      "substrait.Expression.Literal" = {
        if (is.na(x) && !is.nan(x)) {
          substrait$Expression$Literal$create(
            null = substrait_date()
          )
        } else {
          substrait$Expression$Literal$create(date = x)
        }
      },
      NextMethod()
    )
  } else {
    switch(.qualified_name,
      "substrait.Expression.Literal" = {
        substrait$Expression$Literal$create(
          list = substrait$Expression$Literal$List$create(
            values = lapply(x, as_substrait.Date, .ptype = "substrait.Expression.Literal")
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
  switch(.qualified_name,
    "substrait.Type" = {
      type <- names(msg)
      if (length(type) == 0) {
        return(double())
      }

      if (!identical(type, "fp64")) {
        stop(sprintf("Can't convert substrait.Type<%s> to double() ptype", type))
      }
      double()
    },
    "substrait.Expression" = {
      literal <- msg$literal
      if (is.null(literal)) {
        stop("Can't convert non-literal Expression to double()")
      }

      from_substrait(literal, x)
    },
    "substrait.Expression.Literal" = {
      lst <- as.list(msg)
      switch(names(lst)[1],
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
  switch(.qualified_name,
    "substrait.Type" = {
      type <- names(msg)
      if (length(type) == 0) {
        return(integer())
      }

      if (!identical(type, "i32")) {
        stop(sprintf("Can't convert substrait.Type<%s> to integer() ptype", type))
      }
      integer()
    },
    "substrait.Expression" = {
      literal <- msg$literal
      if (is.null(literal)) {
        stop("Can't convert non-literal Expression to integer()")
      }

      from_substrait(literal, x)
    },
    "substrait.Expression.Literal" = {
      lst <- as.list(msg)
      switch(names(lst)[1],
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
  switch(.qualified_name,
    "substrait.Type" = {
      type <- names(msg)
      if (length(type) == 0) {
        return(logical())
      }

      if (!identical(type, "bool")) {
        stop(sprintf("Can't convert substrait.Type<%s> to logical() ptype", type))
      }
      logical()
    },
    "substrait.Expression" = {
      literal <- msg$literal
      if (is.null(literal)) {
        stop("Can't convert non-literal Expression to logical()")
      }

      from_substrait(literal, x)
    },
    "substrait.Expression.Literal" = {
      lst <- as.list(msg)
      switch(names(lst)[1],
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

#' @export
from_substrait.character <- function(msg, x, ...) {
  .qualified_name <- make_qualified_name(msg)
  switch(.qualified_name,
    "substrait.Type" = {
      type <- names(msg)
      if (length(type) == 0) {
        return(character())
      }

      if (!identical(type, "string")) {
        stop(sprintf("Can't convert substrait.Type<%s> to character() ptype", type))
      }
      character()
    },
    "substrait.Expression" = {
      literal <- msg$literal
      if (is.null(literal)) {
        stop("Can't convert non-literal Expression to logical()")
      }

      from_substrait(literal, x)
    },
    "substrait.Expression.Literal" = {
      lst <- as.list(msg)
      switch(names(lst)[1],
        "null" = NA_character_,
        "list" = {
          vapply(lst$list$values, from_substrait, character(1), character())
        },
        as.character(lst[[1]])
      )
    },
    NextMethod()
  )
}

#' @export
from_substrait.Date <- function(msg, x, ...) {
  .qualified_name <- make_qualified_name(msg)
  switch(.qualified_name,
    "substrait.Type" = {
      type <- names(msg)
      if (length(type) == 0) {
        return(as.Date(character()))
      }

      if (!identical(type, "date")) {
        stop(sprintf("Can't convert substrait.Type<%s> to Date ptype", type))
      }
      as.Date(character())
    },
    "substrait.Expression" = {
      literal <- msg$literal
      if (is.null(literal)) {
        stop("Can't convert non-literal Expression to Date")
      }

      from_substrait(literal, x)
    },
    "substrait.Expression.Literal" = {
      lst <- as.list(msg)
      switch(names(lst)[1],
        "null" = NA_real_,
        "list" = {
          out <- vapply(lst$list$values, from_substrait, as.Date(character(1)), as.Date(character()))
          as.Date(out, origin = "1970-01-01")
        },
        as.Date(lst[[1]], origin = "1970-01-01")
      )
    },
    NextMethod()
  )
}

