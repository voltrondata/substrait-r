
#' Substrait compiler
#'
#' @param compiler a [substrait_compiler()]
#' @param name A function name
#' @param arg_types A `list()` of substrait.Type objects
#' @param args A `list()` of arguments, which may be R objects or field
#'   references and may be named (i.e., these have not yet been converted
#'   into substrait.Expression objects).
#' @param context An optional context that can be used to pick the correct
#'   function (e.g., scalar, window, aggregate).
#' @param pkg An R package name or NULL if no R package name was present
#'   in the function call.
#' @param ... Passed to S3 methods.
#'
#' @return
#'   - `substrait_compiler()` returns an object of class "substrait_compiler"
#'   - `substrait_compiler_function_id()` returns an integer identifier for
#'     a function with a given name and argument types
#'   - `substrait_compiler_function()` returns
#'     a "substrait.Expression.ScalarFunction", a
#'     "substrait.Expression.WindowFunction", or a "substrait.AggregateFunction"
#'     depending on the `context`.
#' @export
#'
#' @examples
#' (compiler <- substrait_compiler())
#' substrait_compiler_function_id(compiler, "some_func", list(integer()))
#' substrait_compiler_function(compiler, "some_func", list(1L))
#'
substrait_compiler <- function() {
  compiler <- new.env(parent = emptyenv())
  # use 1 for starting IDs because 0 is the default protobuf value
  compiler$extension_uri <- substrait$extensions$SimpleExtensionURI$create(
    extension_uri_anchor = 1L
  )
  compiler$function_extensions <- new.env(parent = emptyenv())
  compiler$type_extensions <- new.env(parent = emptyenv())
  compiler$type_variations <- new.env(parent = emptyenv())

  compiler$next_id <- 1L

  structure(
    compiler,
    class = "substrait_compiler"
  )
}

#' @rdname substrait_compiler
#' @export
substrait_compiler_function_id <- function(compiler, name, arg_types) {
  arg_types <- unname(lapply(arg_types, as_substrait, "substrait.Type"))
  key <- rlang::hash(list(name, arg_types))

  extension_function <- compiler$function_extensions[[key]]
  if (is.null(extension_function)) {
    extension_function <- compiler$function_extensions[[key]] <- substrait$
      extensions$
      SimpleExtensionDeclaration$
      ExtensionFunction$create(
        extension_uri_reference = compiler$extension_uri$extension_uri_anchor,
        function_anchor = substrait_compiler_next_id(compiler),
        name = name
      )
  }

  extension_function$function_anchor
}

substrait_compiler_next_id <- function(compiler) {
  id <- compiler$next_id
  compiler$next_id <- id + 1L
  id
}

#' @rdname substrait_compiler
#' @export
substrait_compiler_function <- function(compiler, name, args, pkg = NULL,
                                        context = NULL, ...) {
  UseMethod("substrait_compiler_function")
}

#' @export
substrait_compiler_function.substrait_compiler <- function(compiler, name, args, pkg = NULL,
                                                           context = NULL, ...) {
  # resolve arguments as Expressions if they haven't been already
  # (generally they should be already but this will assert that)
  args <- lapply(
    args,
    as_substrait,
    "substrait.Expression",
  )

  # apply some namespacing on the name so that we get some_pkg__some_fun
  prefix <- if (is.null(pkg)) "" else paste0(pkg, "__")
  name <- paste0(prefix, name)

  # resolve argument types (the `context` is needed to resolve the type of
  # field references)
  arg_types <- lapply(args, as_substrait, "substrait.Type", context = context)

  # resolve the function identifier
  id <- substrait_compiler_function_id(compiler, name, arg_types)

  # maybe there's a way to know this later on but for now,
  # leave an unspecified type
  output_type <- substrait$Type$create()

  type <- context$function_type %||% "scalar"
  switch(
    type,
    "scalar" = substrait$Expression$create(
      scalar_function = list(
        function_reference = id,
        args = args,
        output_type = output_type
      )
    ),
    "window" = substrait$Expression$create(
      window_function = list(
        function_reference = id,
        args = args,
        output_type = output_type,
        partitions = context$partitions %||% unspecified(),
        sorts = context$sorts %||% unspecified(),
        upper_bound = context$upper_bound %||% unspecified(),
        lower_bound = context$lower_bound %||% unspecified(),
        phase = context$phase %||% unspecified()
      )
    ),
    "aggregate" = substrait$AggregateFunction$create(
      function_reference = id,
      args = args,
      output_type = output_type,
      sorts = context$sorts %||% unspecified(),
      phase = context$phase %||% unspecified()
    ),
    stop(sprintf("Unknown function type: '%s'", type))
  )
}
