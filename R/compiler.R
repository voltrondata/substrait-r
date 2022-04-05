
#' Substrait Compiler
#'
#' In general, the substrait package provides compiler-agnostic tools to
#' generate `substrait.Plan` objects. However, the consumer that will be
#' evaluating the plan has more information and in many cases has the ability
#' to provide more meaningful validation and output than substrait
#' can provide on its own. The `Compiler` R6 class is a mutable object that
#' provides substrait compilers the ability to customize the behaviour
#' of the [substrait_compiler()] as it is created, modified, printed,
#' and evaluated. While the object itself is mutable, it is cloned whenever
#' the compiler is modified to minimize a user's interaction with reference
#' semantics.
#'
#' @param compiler A [substrait_compiler()]
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
#' @param context Experimental...a portion of the `compiler` needed
#'   to evaluate names and types when evaluating expressions.
#'
#' @export
SubstraitCompiler <- R6::R6Class(
  "SubstraitCompiler",
  public = list(

    #' @field rel The root of the current `substrait.Rel` tree.
    rel = NULL,

    #' @field schema A `substrait.NamedStruct` containing the field names
    #'   and field types of `rel`.
    schema = NULL,

    #' @field mask A named list of `substrait.Expression` objects where the
    #'   names are identical to the field names as provided in `schema`.
    #'   This list is used as the data mask when evaluating expressions
    #'   (e.g., [rlang::eval_tidy()]).
    mask = NULL,

    #' @field groups A named list of `substrait.Expression` to be used for
    #'   future grouping (e.g., after calling [dplyr::group_by()]).
    groups = NULL,


    #' @description
    #' Create a new [SubstraitCompiler].
    #'
    #' @param ... Passed to `add_relation()` if `object` is not `NULL`
    initialize = function(object = NULL, ...) {
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

      # ...and we need unique IDs, so we have a counter
      private$id_counter <- 1L

      # finally, we set the rel, schema, and mask if an object was
      # provided with which to initialize the compiler
      if (!is.null(object)) {
        self$add_relation(object, ...)
      }
    },

    #' @description
    #' Sets the `rel` of this compiler to a `substrait.Rel` (usually a
    #' `substrait.Rel.ReadRel`) and sets `schema` and `mask` to represent
    #' the root of the relation tree.
    #'
    #' @param ... Unused by the default method
    add_relation = function(object, ...) {
      # Technically the substrait.Plan can handle a list() of Rel objects,
      # but we only support one here.
      if (!is.null(self$rel)) {
        stop(
          "More than one relation tree per SubstraitCompiler is not implemented"
        )
      }

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

      self$rel <- rel
      self$schema <- rel$read$base_schema
      self$mask <- substrait_rel_mask(rel)
      self$groups <- NULL

      self
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
    #' Validates a compiler after it was modified. This is an opportunity to
    #' provide meaningful feedback (e.g., errors, warnings)
    #'
    #' @return `self`
    validate = function() {
      self
    },

    #' @description
    #' Evaluates the plan being built by the compiler.
    #'
    #' @param ... Extra arguments specific to the compiler type.
    #'
    #' @return A table-like object whose structure is defined by the
    #'   [SubstraitCompiler] class. The returned object should have a
    #'   [as.data.frame()] method.
    evaluate_compiler = function(compiler, ...) {
      compiler
    },

    #' @description
    #' Resolves an R function call as a Substrait function call.
    #'
    #' @return A modified `template` with `function_reference`,
    #'   `args`, and `output_type` set.
    resolve_function = function(name, args, template) {
      # resolve arguments as Expressions if they haven't been already
      # (generally they should be already but this will assert that)
      args <- lapply(
        args,
        as_substrait,
        "substrait.Expression",
        compiler = self
      )

      # resolve argument types
      arg_types <- lapply(args, as_substrait, "substrait.Type", compiler = self)

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

#' Build a Substrait plan
#'
#' @param object A table-like object with which to create a compiler.
#' @param ... Passed to the [SubstraitCompiler] when creating a new compiler
#'
#' @return An object of class 'substrait_compiler'
#' @export
#'
#' @examples
#' substrait_compiler(data.frame(col1 = 1 , col2 = "one"))
#'
substrait_compiler <- function(object, ...) {
  UseMethod("substrait_compiler")
}

#' @rdname substrait_compiler
#' @export
substrait_compiler.SubstraitCompiler <- function(object, ...) {
  object
}

#' @rdname substrait_compiler
#' @export
substrait_compiler.default <- function(object, ...) {
  SubstraitCompiler$new(object, ...)
}
