
# dd: As the name suggests, we need a `list()` of substrait.Expression objects
# to use as a mask for evaluation (so that symbols map to an Expression, which
# is probably a field reference for now). We also need a `list()` of
# types so that we can convert the type of substrait.Expression that is a
# field reference. I've wrapped both of these into `context` just to make
# something work.
# NOTE!? field indexes have to start at 1 because a value of 0 is the
# default protobuf value?
new_context <- function(x = data.frame()) {
  schema <- as_substrait(x, "substrait.NamedStruct")
  mask <- lapply(
    seq_along(schema$names),
    function(i) substrait$Expression$create(
      selection = list(
        direct_reference = list(
          struct_field = list(
            field = i
          )
        )
      )
    )
  )
  names(mask) <- schema$names

  list(
    schema = schema,
    list_of_expressions = mask
  )
}

# A note that both `context` and `list_of_expressions` needs to be hashed out
# maybe the `list_of_expressions` is what our version of the "unbound table"
# looks like?

#' @export
as_substrait.quosure <- function(x, .ptype = NULL, ..., compiler = substrait_compiler(),
                                 context = NULL) {
  if (is.null(.ptype)) {
    .ptype <- "substrait.Expression"
  }

  .qualified_name <- make_qualified_name(.ptype)
  switch(
    .qualified_name,
    "substrait.Expression" = {
      mask <- context$list_of_expressions

      # evaluate the result using special rules for function calls
      result <- substrait_eval_expr(
        rlang::quo_get_expr(x),
        compiler = compiler,
        context = context,
        env = rlang::quo_get_env(x),
        mask = mask
      )

      # the result might be an atomic R object and not an Expression yet,
      # so convert it to one!
      as_substrait(result, "substrait.Expression")
    },
    NextMethod()
  )
}

substrait_eval_expr <- function(x, compiler, context, env, mask) {
  if (rlang::is_call(x, c("$", "[["))) {
    if (rlang::is_symbol(x[[2]], ".data") && rlang::is_symbol(x[[1]], c("$", "[["))) {
      return(rlang::eval_tidy(x, mask, env))
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
    } else if (is.symbol(fun_expr)) {
      name <- as.character(fun_expr)
    } else {
      return(rlang::eval_tidy(x, mask, env))
    }

    # evaluate the arguments first (because we need the types to resolve
    # the function)
    args <- lapply(
      x[-1],
      substrait_eval_expr,
      compiler = compiler,
      context = context,
      env = env,
      mask = mask
    )

    # resolve the function call as an expression from the compiler
    substrait_compiler_function(
      compiler,
      name,
      args,
      pkg = pkg,
      context = context
    )
  } else {
    rlang::eval_tidy(x, mask, env)
  }
}

#' @export
as_substrait.substrait_Expression <- function(x, .ptype = NULL, ..., context = NULL) {
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
          if (is.null(struct_field$field) || !is.null(struct_field$child)) {
            stop("field reference is not a simple integer reference")
          }

          schema <- context$schema
          if (is.null(schema)) {
            stop("Can't guess field reference type without `context$schema`")
          }

          result <- context$schema$struct_$types[[struct_field$field]]
          if (is.null(result)) {
            stop(sprintf("Index out of bounds [%d]", struct_field$field))
          }

          result
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

