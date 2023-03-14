
#' @export
as_substrait.quosure <- function(x, .ptype = NULL, ...) {
  if (is.null(.ptype)) {
    .ptype <- "substrait.Expression"
  }

  .qualified_name <- make_qualified_name(.ptype)
  switch(.qualified_name,
    "substrait.AggregateRel.Measure" = {
      result <- substrait_eval_quo(x)

      # If we get something other than an AggregateFunction here, we can't
      # return a Measure. Instead, we signal a classed error that contains
      # information that a caller can use to possibly modify the input
      # expression and try again. The most common reason this might happen
      # is if `result` evaluates to a literal value (or function thereof).
      if (!inherits(result, "substrait_AggregateFunction")) {
        rlang::abort(
          "Can't convert non-aggregate expression to AggregateRel.Measure",
          class = "substrait_cant_convert_AggregateRel_Measure",
          substrait_x = result
        )
      }

      substrait$AggregateRel$Measure$create(
        measure = as_substrait(result, "substrait.AggregateFunction")
      )
    },
    "substrait.SortField" = {
      result <- substrait_eval_quo(x)

      # ...but wrap result in SortField if it isn't already one
      if (inherits(result, "substrait_SortField")) {
        result
      } else {
        substrait$SortField$create(
          expr = as_substrait_expression(result),
          direction = "SORT_DIRECTION_ASC_NULLS_LAST"
        )
      }
    },
    "substrait.Expression" = {
      as_substrait_expression(substrait_eval_quo(x))
    },
    NextMethod()
  )
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

          if (field < 0 || field >= length(compiler$schema$struct$types)) {
            stop(sprintf("Field reference out of bounds [%d]", field))
          }

          # because field is zero-indexed
          compiler$schema$struct$types[[field + 1]]
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
    "substrait.FunctionArgument" = substrait$FunctionArgument$create(value = x),
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
    .ptype <- x
  }

  switch(make_qualified_name(.ptype),
    "substrait.Expression" = substrait$Expression$create(scalar_function = x),
    NextMethod()
  )
}

#' @export
as_substrait.substrait_FunctionArgument <- function(x, .ptype = NULL, ...) {
  if (is.null(.ptype)) {
    .ptype <- x
  }

  switch(make_qualified_name(.ptype),
    "substrait.Type" = {
      if (!is.null(x$value)) {
        as_substrait(x$value)
      } else {
        substrait$Type$create()
      }
    },
    NextMethod()
  )
}

#' @export
as_substrait.substrait_AggregateFunction <- function(x, .ptype = NULL, ...) {
  if (is.null(.ptype)) {
    .ptype <- x
  }

  switch(make_qualified_name(.ptype),
    "substrait.Expression" = {
      # This happens when, during expression evaluation, one of the arguments
      # that is supposed to be a substrait.Expression is actually the result
      # of an aggregate expression (e.g., sum(x) + 1). We can return a field
      # reference here: instead, signal a condition with information the caller
      # might be able to use to fix the input expression.
      rlang::abort(
        "Can't convert AggregateFunction to Expression",
        class = "substrait_cant_convert_Expression",
        substrait_x = x
      )
    },
    NextMethod()
  )
}
