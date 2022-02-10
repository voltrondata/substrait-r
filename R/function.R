
#' Resolve function references
#'
#' @param name The function name
#' @param type One of "scalar", "aggregate" or "windows"
#' @param args A vector of (possibly named) arguments used to resolve
#'   an implementation.
#' @inheritParams register_functions
#'
#' @return A Substrait message object of type Expression.ScalarFunction,
#'   Expression.WindowFunction, or AggregateFunction.
#' @export
#'
resolve_function_by_name <- function(name, args,
                                     registry = default_function_registry(),
                                     type = "scalar") {
  func <- registry[[type]][[name]]
  if (is.null(func)) {
    stop(sprintf("No such function: '%s'", name))
  }

  # at this point in the evaluation, all the args should be a typed
  # field reference or typed literals.
  args <- lapply(args, as_substrait, .ptype = "substrait.Expression")

  # for now just return the first implementation (no type or name matching)
  impl <- func$impls[[1]]
  if (is.null(impl)) {
    stop(sprintf("Function '%s' has zero implementations", name))
  }

  # bit of a hack to get a Type object from the yaml-defined thinger
  return_type_str <- impl$return
  output_type <- substrait_create("substrait.Type", !! return_type_str := list())

  switch(
    type,
    "scalar" = substrait$Expression$create(
      scalar_function = substrait$Expression$ScalarFunction$create(
        function_reference = func$.function_reference,
        args = args,
        output_type = output_type
      )
    ),
    stop(sprintf("Function type '%s' is not yet supported", type))
  )
}

#' Register functions
#'
#' @param scalar A named list of scalar function definitions
#' @param aggregate A named list of aggregate function definitions
#' @param window A list of window function definitions
#' @param registry The [default_function_registry()] or a custom registry
#'   created using [new_function_registry()].
#' @param path A path to .yaml function definitions.
#'
#' @return
#'   - `register_function()` and `register_functions_yaml()` return `registry`,
#'     (invisibly)
#'   - `default_function_registry()` and `new_function_registry()` return a
#'     `list()` with items "scalar", "window", and "aggregate".
#' @export
#'
#' @examples
#' default_function_registry()
#' (registry <- new_function_registry())
#'
register_functions <- function(scalar = list(), aggregate = list(),
                               window = list(),
                               registry = default_function_registry()) {
  scalar <- as.list(scalar)
  aggregate <- as.list(aggregate)
  window <- as.list(window)

  if (is.null(names(scalar))) {
    names(scalar) <- rep("", length(scalar))
  }

  if (is.null(names(aggregate))) {
    names(aggregate) <- rep("", length(aggregate))
  }

  if (is.null(names(window))) {
    names(window) <- rep("", length(window))
  }

  for (i in seq_along(scalar)) {
    def <- scalar[[i]]
    name <- if (names(scalar)[i] == "") def$name else names(scalar)[i]
    register_or_add_impls(name, def, registry, "scalar")
  }

  for (i in seq_along(aggregate)) {
    def <- aggregate[[i]]
    name <- if (names(scalar)[i] == "") def$name else names(scalar)[i]
    register_or_add_impls(name, def, registry, "aggregate")
  }

  for (i in seq_along(window)) {
    def <- window[[i]]
    name <- if (names(scalar)[i] == "") def$name else names(scalar)[i]
    register_or_add_impls(name, def, registry, "window")
  }

  invisible(registry)
}

register_or_add_impls <- function(name, def, registry, type) {
  existing_def <- registry[["scalar"]][[name]]
  if (is.null(existing_def)) {
    def$.function_reference <- registry$.next_function_reference
    registry$.next_function_reference <- registry$.next_function_reference + 1L
    registry[[type]][[name]] <- def
  } else {
    registry[[type]][[name]]$impls <- c(
      existing_def$impls,
      registry[[type]][[name]]$impls
    )
  }
}

#' @rdname register_functions
#' @export
register_functions_yaml <- function(path, registry = default_function_registry()) {
  for (file in path) {
    # because YAML 1.2 is not supported by the yaml package and the
    # % YAML 1.2 header line causes a syntax error
    con <- file(file, encoding = "UTF-8")
    lines <- try(readLines(con), silent = TRUE)
    close(con)
    if (inherits(lines, "try-error")) {
      stop(as.character(lines), call. = FALSE)
    }

    lst <- yaml::read_yaml(
      text = paste0(lines[-(1:2)], collapse = "\n"),
      eval.expr = FALSE
    )

    register_functions(
      scalar = lst$scalar_functions,
      aggregate = lst$aggregate_functions,
      window = lst$window_functions,
      registry = registry
    )
  }

  invisible(registry)
}

#' @rdname register_functions
#' @export
default_function_registry <- function() {
  global_function_registry
}

#' @rdname register_functions
#' @export
new_function_registry <- function() {
  # following the structure of dbplyr::dbplyr_sql_translation()
  lst <- list(
    scalar = new.env(parent = emptyenv()),
    aggregate = new.env(parent = emptyenv()),
    window = new.env(parent = emptyenv()),
    .next_function_reference = 1L
  )

  structure(
    as.environment(lst),
    class = "substrait_function_registry"
  )
}

global_function_registry <- new_function_registry()
