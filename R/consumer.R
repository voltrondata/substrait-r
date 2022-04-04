
#' Substrait Consumer
#'
#' In general, the substrait package provides consumer-agnostic tools to
#' generate `substrait.Plan` objects. However, the consumer that will be
#' evaluating the plan has more information and in many cases has the ability
#' to provide more meaningful error messages and/or previews than substrait
#' can provide on its own. The `Consumer` R6 class is a mutable object that
#' provides substrait consumers the ability to customize the behaviour
#' of the [substrait_builder()] as it is created, modified, printed,
#' and evaluated. While the object itself is mutable, it is cloned whenever
#' the builder is modified to minimize a user's interaction with reference
#' semantics. Whereas the [Consumer] defines the interface that other substrait
#' package functions use, the [GenericConsumer] provides an implementation
#' that takes care of a number of useful details that implementors may wish
#' to use.
#'
#' @param builder A [substrait_builder()]
#' @param object An object, most commonly a data.frame or table-like
#'   object.
#' @param name The fully-qualified name of the function as it was
#'   called (e.g., `pkg::fun`). If no package name was explicitly
#'   specified, the package name will not be present in `name`.
#' @param args A `list()` of arguments. These may be R objects or Substrait
#'   objects created while evaluating the user-provided arguments
#'   (e.g., field references or function calls).
#' @param template A `substrait.Expression.ScalarFunction`, a
#'   `substrait.Expression.WindowFunction`, or a
#'   `substrait.AggregateFunction`.
#'
#' @export
Consumer <- R6::R6Class(
  "Consumer",
  public = list(

    # nocov start

    #' @description
    #' Creates a new [substrait_builder()] instance from a given `object`.
    #'
    #' @param ... Extra arguments specific to the [Consumer]/`object`.
    #'
    #' @return A [substrait_builder()].
    create_builder = function(object, ...) {
      stop("Not implemented")
    },

    #' @description
    #' Validates a builder after it was modified. This is an opportunity to
    #' provide meaningful feedback after the `builder` has been modified.
    #'
    #' @return `builder`, potentially modified
    validate_builder = function(builder) {
      stop("Not implemented")
    },

    #' @description
    #' Evaluates the plan being built by `builder`.
    #'
    #' @return A table-like object whose structure is defined by the [Consumer]
    #'   class. The returned object should have a [as.data.frame()] method.
    collect_builder = function(builder) {
      stop("Not implemented")
    },

    #' @description
    #' Prints a preview the plan being built by `builder`
    #'
    #' @param ... The dots passed to [print()]
    print_builder = function(builder, ...) {
      stop("Not implemented")
    },

    #' @description
    #' Resolves an R function call as a Substrait function call.
    #'
    #' @return A modified `template`.
    resolve_function = function(builder, name, args, template) {
      stop("Not implemented")
    }

    # nocov end
  )
)

#' @rdname Consumer
#'
#' @param builder A [substrait_builder()]
#' @param object An object, most commonly a data.frame or table-like
#'   object.
#' @param name The fully-qualified name of the function as it was
#'   called (e.g., `pkg::fun`). If no package name was explicitly
#'   specified, the package name will not be present in `name`.
#' @param args A `list()` of arguments. These may be R objects or Substrait
#'   objects created while evaluating the user-provided arguments
#'   (e.g., field references or function calls).
#' @param template A `substrait.Expression.ScalarFunction`, a
#'   `substrait.Expression.WindowFunction`, or a
#'   `substrait.AggregateFunction`.
#'
#' @export
GenericConsumer <- R6::R6Class(
  "GenericConsumer", inherit = Consumer,
  public = list(

    #' @description
    #' Create a new [GenericConsumer]
    initialize = function() {
      private$extension_uri <- substrait$extensions$SimpleExtensionURI$create(
        extension_uri_anchor = 1L
      )

      # these are key/value stores but for at least function_extensions
      # we need to keep the keys as well as the values
      private$function_extensions <- list()
      private$function_extensions_key <- list()

      private$type_extensions <- list()
      private$type_variations <- list()

      # we also use this object to keep track of named tables
      private$named_tables <- list()

      private$id_counter <- 1L
    },

    #' @description
    #' Implementation of `Consumer$create_builder()`.
    #'
    #' @param ... Unused
    create_builder = function(object, ...) {
      tbl_id <- sprintf("named_table_%d", self$next_id())

      rel <- substrait$Rel$create(
        read = substrait$ReadRel$create(
          base_schema = as_substrait(object, "substrait.NamedStruct"),
          named_table = substrait$ReadRel$NamedTable$create(
            names = tbl_id
          )
        )
      )

      private$named_tables[[tbl_id]] <- object

      new_substrait_builder(
        list(
          rel = rel,
          consumer = self,
          schema = rel$read$base_schema,
          mask = substrait_rel_mask(rel),
          groups = NULL
        )
      )
    },

    #' @description
    #' Retrieve a named table
    #'
    #' @param name A table name
    #'
    #' @return The `object` that was passed
    named_table = function(name) {
      private$named_tables[[name]]
    },

    #' @description
    #' Implementation of `Consumer$validate_builder()`.
    validate_builder = function(builder) {
      builder
    },

    #' @description
    #' Implementation of `Consumer$collect_builder()`.
    collect_builder = function(builder) {
      builder
    },

    #' @description
    #' Implementation of `Consumer$resolve_function()`.
    resolve_function = function(name, args, template, builder = NULL) {
      # resolve arguments as Expressions if they haven't been already
      # (generally they should be already but this will assert that)
      args <- lapply(
        args,
        as_substrait,
        "substrait.Expression",
      )

      # resolve argument types (the `context` is needed to resolve the type of
      # field references)
      context <- list(
        schema = builder$schema,
        list_of_expressions = builder$mask
      )

      arg_types <- lapply(args, as_substrait, "substrait.Type", context = context)

      # resolve the function identifier
      id <- self$function_id(name, arg_types)

      # maybe there's a way to know this later on but for now,
      # leave an unspecified type
      output_type <- substrait$Type$create()

      template$function_reference <- id
      template$args <- args
      template$output_type <- output_type

      template
    },

    #' @details
    #' Get a function reference identifier for a given function/input
    #' argument combination.
    #'
    #' @param arg_types A `list()` of `substrait.Type` objects.
    #' @return An integer function reference
    function_id = function(name, arg_types) {
      arg_types <- unname(lapply(arg_types, as_substrait, "substrait.Type"))
      key <- list(name = name, arg_types = arg_types)
      key_hash <- rlang::hash(key)

      extension_function <- private$function_extensions[[key_hash]]
      if (is.null(extension_function)) {
        extension_function <- private$function_extensions[[key_hash]] <- substrait$
          extensions$
          SimpleExtensionDeclaration$
          ExtensionFunction$create(
            extension_uri_reference = private$extension_uri$extension_uri_anchor,
            function_anchor = self$next_id(),
            name = name
          )

        id_chr <- as.character(extension_function$function_anchor)
        private$function_extensions_key[[id_chr]] <- key
      }

      extension_function$function_anchor
    },

    #' @description
    #' Retrieve a function extension by anchor/reference
    #'
    #' @param id An function_anchor/function_reference identifier
    #'
    #' @return A
    #'   `substrait.extensions.SimpleExtensionDeclaration.ExtensionFunction`.
    function_extension = function(id) {
      private$function_extensions_key[[as.character(id)]]
    },

    #' @description
    #' Get the next unique identifier.
    #'
    #' @return An integer that has not been returned by a previous call to
    #'   `next_id()` for this instance.
    next_id = function() {
      id <- private$id_counter
      private$id_counter <- id + 1L
      id
    }
  ),

  private = list(
    extension_uri = NULL,
    function_extensions = NULL,
    function_extensions_key = NULL,
    type_extensions = NULL,
    type_variations = NULL,
    named_tables = NULL,
    id_counter = NULL
  )
)
