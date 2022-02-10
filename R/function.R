

resolve_function_scalar <- function(name, args = list(),
                                    registry = default_function_registry()) {

}

resolve_function_window <- function(name, args = list(),
                                    registry = default_function_registry()) {
  stop("Not implemented")
}

resolve_function_aggregate <- function(name, args = list(),
                                       registry = default_function_registry()) {
  stop("Not implemented")
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
  stop("Not implemented")
}

#' @rdname register_functions
#' @export
register_functions_yaml <- function(path, registry = default_function_registry()) {
  stop("Not implemented")
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
  list(
    scalar = new.env(parent = emptyenv()),
    aggregate = new.env(parent = emptyenv()),
    window = new.env(parent = emptyenv())
  )
}

global_function_registry <- new_function_registry()
