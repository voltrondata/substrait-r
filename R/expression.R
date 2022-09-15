
#' @export
as_substrait.quosure <- function(x, .ptype = NULL, ...,
                                 template = substrait$Expression$ScalarFunction$create()) {
  if (is.null(.ptype)) {
    .ptype <- "substrait.Expression"
  }

  .qualified_name <- make_qualified_name(.ptype)
  switch(.qualified_name,
    "substrait.AggregateRel.Measure" = {
      # evaluate the result using special rules for function calls
      result <- substrait_eval_expr(
        rlang::quo_get_expr(x),
        compiler = current_compiler(),
        env = rlang::quo_get_env(x),
        template = template
      )

      # Allow caller to handle something that isn't an aggregate function
      # (e.g., by adding a post-mutate step)
      if (!inherits(result, "substrait_AggregateFunction")) {

      }

      # The only valid result here is an AggregateFunction (other values
      # aren't yet supported)
      substrait$AggregateRel$Measure$create(
        measure = as_substrait(result, "substrait.AggregateFunction")
      )
    },
    "substrait.SortField" = {
      # evaluate the result using special rules for function calls
      result <- substrait_eval_quo(x)

      # ...but wrap result in SortField if it isn't already one
      if (inherits(result, "substrait_SortField")) {
        result
      } else {
        substrait$SortField$create(
          expr = as_substrait(result, "substrait.Expression"),
          direction = "SORT_DIRECTION_ASC_NULLS_LAST"
        )
      }
    },
    "substrait.Expression" = {
      as_substrait(substrait_eval_quo(x), "substrait.Expression")
    },
    NextMethod()
  )
}

substrait_eval_expr <- function(x, compiler, env, template) {
  # Handle .data and .env pronouns that are also supported in dplyr's
  # tidy evaluation. .data$some_var must match columns by name;
  # .env$var_name looks explicitly in the calling environment
  # (i.e., explicitly does not match a column named var_name).
  if (rlang::is_call(x, c("$", "[["))) {
    if (rlang::is_symbol(x[[2]], ".data") && rlang::is_symbol(x[[1]], c("$", "[["))) {
      # rlang already has support for the .data pronoun built in, so we can
      # continue with eval_tidy() and let rlang interpret .data$some_var
      return(rlang::eval_tidy(x, compiler$.data, env))
    } else if (rlang::is_symbol(x[[2]], ".env") && rlang::is_symbol(x[[1]], "$")) {
      # If we have .env$some_var, the righthand side is passed as a symbol,
      # so look up the variable in env based on that. We include inheriting
      # environments in the lookup because dplyr does this, too. This could
      # or maybe should be implemented using native rlang data pronoun
      # behaviour (I believe dplyr does it this way).
      key <- as.character(x[[3]])
      return(get(key, envir = env, inherits = TRUE))
    } else if (rlang::is_symbol(x[[2]], ".env") && rlang::is_symbol(x[[1]], "[[")) {
      # If we have .env[[some_expr]], evaluate some_expr without the data mask.
      key <- as.character(rlang::eval_tidy(x[[3]], env = env))
      return(get(key, envir = env, inherits = TRUE))
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
      return(rlang::eval_tidy(x, compiler$.data, env))
    }

    # Evaluate the arguments first because we need the types to resolve
    # the function. For aggregations, we don't pass on the template (e.g.,
    # in sum(x + 1), `+` is not an aggregate function)
    args <- lapply(
      x[-1],
      substrait_eval_expr,
      compiler = compiler,
      env = env,
      template = if (inherits(template, "substrait_AggregateFunction")) {
        substrait$Expression$ScalarFunction$create()
      } else {
        template
      }
    )

    # Resolve the function call as an expression from the compiler.
    # The return value could be another type of expression (e.g., IfThen)
    # depending on the function, or an AggregateFunction if we're
    # aggregating
    result <- compiler$resolve_function(name, args, template)

    if (inherits(template, "substrait_AggregateFunction")) {
      as_substrait(result, "substrait.AggregateFunction")
    } else {
      as_substrait(result, "substrait.Expression")
    }
  } else {
    rlang::eval_tidy(x, compiler$.data, env)
  }
}

#' @export
as_substrait.substrait_Expression <- function(x, .ptype = NULL, ...) {
  if (is.null(.ptype)) {
    .ptype <- x
  }

  switch(make_qualified_name(.ptype),
    "substrait.Type" = {
      which_expr_type <- names(x)
      if (length(which_expr_type) == 0) {
        return(substrait$Type$create())
      }

      guessed_type <- switch(which_expr_type,
        "literal" = as_substrait(x[[which_expr_type]], .ptype),
        "selection" = {
          compiler <- current_compiler()
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

#' @export
as_substrait.substrait_AggregateRel_Measure <- function(x, .ptype = NULL, ...) {
  if (is.null(.ptype)) {
    .ptype <- x
  }

  switch(make_qualified_name(.ptype),
    "substrait.Type" = x$measure$output_type %||% substrait$Type$create(),
    NextMethod()
  )
}

#' @export
as_substrait.substrait_Expression_ScalarFunction <- function(x, .ptype = NULL, ...) {
  if (is.null(.ptype)) {
    return(x)
  }

  switch(make_qualified_name(.ptype),
    "substrait.Expression" = substrait$Expression$create(scalar_function = x),
    NextMethod()
  )
}
