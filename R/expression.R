
#' @export
as_substrait.quosure <- function(x, .ptype = NULL, ...,
                                 functions = default_function_registry(),
                                 fields = list(),
                                 function_type = "scalar",
                                 mask = NULL) {
  if (is.null(.ptype)) {
    .ptype <- "substrait.Expression"
  }

  .qualified_name <- make_qualified_name(.ptype)
  switch(
    .qualified_name,
    "substrait.Expression" = {
      expr <- as_substrait(rlang::quo_get_expr(), "substrait.Expression")
      rlang::eval_tidy(rlang::quo_set_expr(x, expr))
    },
    NextMethod()
  )
}

#' @export
as_substrait.call <- function(x, .ptype = NULL, ...,
                              functions = default_function_registry(),
                              fields = default_field_registry(),
                              function_type = "scalar",
                              env = parent.frame(),
                              mask = NULL) {
  if (is.null(.ptype)) {
    .ptype <- "substrait.Expression"
  }

  .qualified_name <- make_qualified_name(.ptype)
  switch(
    .qualified_name,
    "substrait.Expression" = {
      # the name of the function
      fun_expr <- x[[1]]
      if (!is.name(fun_expr)) {
        stop(
          sprintf(
            "Error in `%s`: Can't resolve function that is not a symbol",
            format(x)
          )
        )
      }

      # evaluate the arguments into substrait objects
      tryCatch({
        args_substrait <- lapply(
          args[-1],
          as_substrait,
          functions = functions,
          fields = fields,
          function_type = function_type,
          env = env,
          mask = mask
        )

        fun <- resolve_function_by_name(
          name = as.character(fun_expr),
          args = args_substrait,
          registry = functions,
          type = function_type
        )

        switch(
          function_type,
          "scalar" = substrait$Expression$create(
            scalar_function = substrait$Expression$ScalarFunction$create(
              function_reference = fun$function_reference,
              args = args_substrait,
              output_type = fun$output_type
            )
          ),
          stop(sprintf("Function type '%s' is not yet supported", function_type))
        )
      }, error = function(e) {
        rlang::abort(
          "Error converting call to Substrait",
          call = x, parent = e
        )
      })
    },
    NextMethod()
  )
}

#' @export
as_substrait.name <- function(x, .ptype = NULL, ...,
                              fields = list(),
                              env = parent.frame(),
                              mask = NULL) {
  if (is.null(.ptype)) {
    .ptype <- "substrait.Expression"
  }

  .qualified_name <- make_qualified_name(.ptype)
  switch(
    .qualified_name,
    "substrait.Expression" = {
      if (as.character(x) %in% names(fields)) {
        fields[[as.character(x)]]
      } else {
        as_substrait(rlang::eval_tidy(x, mask, env), "substrait.Expression")
      }
    },
    NextMethod()
  )
}
