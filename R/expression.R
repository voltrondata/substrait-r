
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
      expr <- as_substrait(
        rlang::quo_get_expr(x),
        "substrait.Expression",
        env = rlang::quo_get_env(x),
        mask = mask,
        function_type = function_type,
        fields = fields,
        functions = functions
      )

      rlang::eval_tidy(rlang::quo_set_expr(x, expr), data = mask)
    },
    NextMethod()
  )
}

#' @export
as_substrait.call <- function(x, .ptype = NULL, ...,
                              functions = default_function_registry(),
                              fields = list(),
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
      tryCatch({
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
        args_substrait <- lapply(
          x[-1],
          as_substrait,
          "substrait.Expression",
          functions = functions,
          fields = fields,
          function_type = function_type,
          env = env,
          mask = mask
        )

        resolve_function_by_name(
          name = as.character(fun_expr),
          args = args_substrait,
          registry = functions,
          type = function_type
        )
      }, error = function(e) {
        rlang::abort(
          sprintf("Error in `%s`", format(x)),
          parent = e
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
