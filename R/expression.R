
#' @export
as_substrait.quosure <- function(x, .ptype = NULL, ..., compiler = SubstraitCompiler$new()) {
  if (is.null(.ptype)) {
    .ptype <- "substrait.Expression"
  }

  .qualified_name <- make_qualified_name(.ptype)
  switch(
    .qualified_name,
    "substrait.SortField" = {
      # evaluate the result using special rules for function calls
      result <- substrait_eval_expr(
        rlang::quo_get_expr(x),
        compiler = compiler,
        env = rlang::quo_get_env(x)
      )

      # ...but wrap result in SortField if it isn't already one
      if (inherits(result, "substrait_SortField")) {
        result
      } else {
        substrait$SortField$create(
          expr = as_substrait(result, "substrait.Expression")
        )
      }
    },
    "substrait.Expression" = {
      # evaluate the result using special rules for function calls
      result <- substrait_eval_expr(
        rlang::quo_get_expr(x),
        compiler = compiler,
        env = rlang::quo_get_env(x)
      )

      # the result might be an atomic R object and not an Expression yet,
      # so convert it to one!
      as_substrait(result, "substrait.Expression")
    },
    NextMethod()
  )
}

substrait_eval_expr <- function(x, compiler, env) {
  if (rlang::is_call(x, c("$", "[["))) {
    if (rlang::is_symbol(x[[2]], ".data") && rlang::is_symbol(x[[1]], c("$", "[["))) {
      return(rlang::eval_tidy(x, compiler$mask, env))
    }
  }

  if (rlang::is_call(x)) {
    # resolve the function and package (or if it's an inline function,
    # evaluate it)
    fun_expr <- x[[1]]
    pkg <- NULL
    name <- NULL

    if (rlang::is_call(fun_expr, "::")) {
      pkg <- as.character(fun_expr[[2]])
      name <- as.character(fun_expr[[3]])
      name <- paste0(pkg, "::", name)
    } else if (is.symbol(fun_expr)) {
      name <- as.character(fun_expr)
    } else {
      return(rlang::eval_tidy(x, compiler$mask, env))
    }

    # evaluate the arguments first (because we need the types to resolve
    # the function)
    args <- lapply(
      x[-1],
      substrait_eval_expr,
      compiler = compiler,
      env = env
    )

    # resolve the function call as an expression from the compiler
    # only scalar functions for now
    template <- substrait$Expression$ScalarFunction$create()
    fun <- compiler$resolve_function(name, args, template)
    substrait$Expression$create(scalar_function = fun)
  } else {
    rlang::eval_tidy(x, compiler$mask, env)
  }
}

#' @export
as_substrait.substrait_Expression <- function(x, .ptype = NULL, ..., compiler = NULL) {
  if (is.null(.ptype)) {
    .ptype <- x
  }

  switch(
    make_qualified_name(.ptype),
    "substrait.Type" = {
      which_expr_type <- names(x)
      if (length(which_expr_type) == 0) {
        return(substrait$Type$create())
      }

      guessed_type <- switch(
        which_expr_type,
        "literal" = as_substrait(x[[which_expr_type]], .ptype),
        "selection" = {
          struct_field <- x$selection$direct_reference$struct_field
          schema <- compiler$schema
          if (is.null(schema)) {
            stop("Can't guess field reference type without `compiler$schema`")
          }

          # because 0 is the default value
          field <- struct_field$field %||% 0L

          if (field < 0 || field >= length(compiler$schema$struct_$types)) {
            stop(sprintf("Field reference out of bounds [%d]", field))
          }

          # because field is zero-indexed
          compiler$schema$struct_$types[[field + 1]]
        },
        "scalar_function" = x$scalar_function$output_type %||% substrait$Type$create(),
        "window_function" = x$window_function$output_type %||% substrait$Type$create(),
        "cast" = x$cast$type %||% substrait$Type$create(),
        # return an unknown type by default
        substrait$Type$create()
      )

      requested_type <- names(.ptype)
      if (length(requested_type) > 0) {
        stopifnot(identical(requested_type, names(guessed_type)))
      }

      guessed_type
    },
    NextMethod()
  )
}
