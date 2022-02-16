
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

substrait_compiler_function_id <- function(compiler, name, arg_types, pkg = NULL) {
  arg_types <- lapply(arg_types, as_substrait, "substrait.Type")
  key <- rlang::hash(list(pkg, name, arg_types))

  extension_function <- compiler$function_extensions[[key]]
  if (is.null(extension_function)) {
    extension_function <- compiler$function_extensions[[key]] <- substrait$
      extensions$
      SimpleExtensionDeclaration$
      ExtensionFunction$create(
        extension_uri_reference = compiler$extension_uri$extension_uri_anchor,
        function_anchor = substrait_compiler_next_id(compiler),
        name = paste0(pkg %||% "_", "__", name)
      )
  }

  extension_function$function_anchor
}

substrait_compiler_next_id <- function(compiler) {
  id <- compiler$next_id
  compiler$next_id <- id + 1L
  id
}
