
#' Substrait Compiler
#'
#' The [SubstraitCompiler] defines a mutable object that accumulates information
#' needed to evaluate a `substrait.Rel` tree. In addition to the
#' `substrait.Rel` tree itself, the compiler must keep track of function
#' identifiers, column names, and the R objects (e.g., data frames) that
#' will be used as leaf nodes when the plan is evaluated. Specific consumers
#' will need to subclass the [SubstraitCompiler] and implement the `$evaluate()`
#' and/or `$resolve_function()` methods. Typically users will not interact
#' with R6 methods but will use the pipeable interface
#' (e.g. [substrait_select()]). The pipeable interface clones the compiler
#' before it is modified to minimize the user's interaction to R6 reference
#' semantics.
#'
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
#' @param output_type An explicit output type to use or a function accepting
#'   one type per `args`.
#' @param options An optional list of `substrait.FunctionOptions` message
#'   specifying function options for this call.
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

    #' @field .data An environment or list containing `substrait.Expression`
    #'   objects. For a fresh compiler, this will be a list of field references
    #'   with the same length as `schema$names`; however, during evaluation
    #'   this may be updated to contain temporary columns before a relation
    #'   is finalized.
    .data = NULL,

    #' @field .fns An environment or list containing functions that can be
    #'   translated by this compiler.
    .fns = NULL,

    #' @field groups A named list of `substrait.Expression` to be used for
    #'   future grouping (e.g., after calling [dplyr::group_by()]).
    groups = NULL,


    #' @description
    #' Create a new [SubstraitCompiler].
    #'
    #' @param ... Passed to `add_relation()` if `object` is not `NULL`
    initialize = function(object = NULL, ...) {
      self$.fns <- list()

      private$extension_uri <- list(
        substrait$extensions$SimpleExtensionURI$create(extension_uri_anchor = 1L)
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
    #' Returns the [data mask][rlang::as_data_mask] that will be
    #' used within `substrait_eval()`.
    #'
    #' @param .data Use `FALSE` to return a mask containing only function
    #'   members with no columns.
    eval_mask = function(.data = TRUE) {
      column_mask <- if (.data) as.environment(self$.data) else new.env(parent = emptyenv())
      function_mask <- as.environment(as.list(self$.fns))
      parent.env(column_mask) <- function_mask

      mask <- rlang::new_data_mask(column_mask, top = function_mask)
      mask$.data <- rlang::as_data_pronoun(column_mask)
      mask$.fns <- rlang::as_data_pronoun(function_mask)

      mask
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
      self$.data <- substrait_rel_mask(rel)
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
    #' Retrieve all named tables as a [list()]
    #'
    #' @return a named [list()] of objects
    named_table_list = function() {
      private$named_tables
    },

    #' @description
    #' Validates a compiler after it was modified. This is an opportunity to
    #' provide meaningful feedback (e.g., errors, warnings)
    #'
    #' @return `self`
    validate = function() {
      # DuckDB backend doesn't accept empty SELECT clause
      if (inherits(self, "DuckDBSubstraitCompiler") && length(self$schema$names) == 0) {
        rlang::abort("Column list must not be empty")
      }

      self
    },

    #' @description
    #' Assembles a `substrait.Plan` from the current information available
    #' to the compiler.
    #'
    #' @return A `substrait.Plan`
    #'
    plan = function() {
      substrait$Plan$create(
        relations = list(
          substrait$PlanRel$create(
            root = substrait$RelRoot$create(
              input = self$rel,
              names = self$schema$names
            )
          )
        ),
        extension_uris = private$extension_uri,
        extensions = c(
          lapply(
            unname(private$function_extensions),
            function(x) {
              substrait$extensions$SimpleExtensionDeclaration$create(
                extension_function = x
              )
            }
          )
        )
      )
    },

    #' @description
    #' Evaluates the plan being built by the compiler.
    #'
    #' @param ... Extra arguments specific to the compiler type.
    #'
    #' @return A table-like object whose structure is defined by the
    #'   [SubstraitCompiler] class. The returned object should have a
    #'   [as.data.frame()] method.
    evaluate = function(...) {
      stop("Not implemented")
    },

    #' @description
    #' Resolves an R function call as a Substrait function call.
    #'
    #' @return A modified `template` with `function_reference`,
    #'   `args`, and `output_type` set.
    resolve_function = function(name, args, template, output_type = NULL, options = NULL) {
      # resolve arguments as Expressions if they haven't been already
      # (generally they should be already but this will assert that)
      is_function_arg <- vapply(args, inherits, logical(1), "substrait_FunctionArgument")
      args[!is_function_arg] <- lapply(
        args[!is_function_arg],
        as_substrait,
        "substrait.Expression"
      )

      # ...then resolve them as FunctionArguments
      args <- lapply(args, as_substrait, "substrait.FunctionArgument")

      # resolve argument types
      arg_types <- lapply(args, as_substrait_type, compiler = self)

      # resolve the function identifier
      id <- self$function_id(name, arg_types)

      if (is.null(output_type)) {
        output_type <- substrait$Type$create()
      } else if (is.function(output_type)) {
        output_type <- do.call(output_type, arg_types)
      }

      template$function_reference <- id
      template$arguments <- args
      if (!is.null(options)) {
        template$options <- lapply(options, as_substrait, "substrait.FunctionOption")
      }
      template$output_type <- as_substrait_type(output_type)

      template
    },

    #' @details
    #' Get a function reference identifier for a given function/input
    #' argument combination.
    #'
    #' @param arg_types A `list()` of `substrait.Type` objects.
    #' @return An integer function reference
    function_id = function(name, arg_types) {
      arg_types <- unname(lapply(arg_types, as_substrait_type))
      key <- list(name = name, arg_types = arg_types)
      key_hash <- rlang::hash(key)

      extension_function <- private$function_extensions[[key_hash]]
      if (is.null(extension_function)) {
        extension_function <- private$function_extensions[[key_hash]] <- substrait$
          extensions$
          SimpleExtensionDeclaration$
          ExtensionFunction$create(
          extension_uri_reference = self$extension_uri_anchor(name),
          function_anchor = self$next_id(),
          name = name
        )

        id_chr <- as.character(extension_function$function_anchor)
        private$function_extensions_key[[id_chr]] <- key
      }

      extension_function$function_anchor
    },

    #' @description
    #' Get the extension uri anchor value for a given function
    #'
    #' @param name The name of the function
    #'
    #' @return The uri anchor value
    extension_uri_anchor = function(name) {
      private$extension_uri[[1]]$extension_uri_anchor
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

#' Initialize a Substrait Compiler
#'
#' Creates a [SubstraitCompiler] instance initialized with `object`
#' (e.g., a `data.frame()`).
#'
#' @param object A table-like object with which to create a compiler.
#' @param ... Passed to the [SubstraitCompiler] when creating a new compiler
#'
#' @return An object of class 'substrait_compiler'
#' @export
#'
#' @examples
#' substrait_compiler(data.frame(col1 = 1, col2 = "one"))
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

#' Translation utilities
#'
#' These functions are used to translate R function calls into Substrait
#' expressions that can be passed on to a consumer, compiled, and evaluated
#' there. Use [substrait_call()] to generate a substrait.Expression.ScalarFunction
#' based on a function name and a series of arguments; use [substrait_eval()]
#' to translate R code using Substrait function translations where possible;
#' and use [substrait_eval_data()] translate R code using Substrait function
#' translations *and* the `.data` mask from the [current_compiler()].
#'
#' @param .fun The name of a substrait function as a string that will
#'   be passed on to the consumer. The function will be registered with
#'   the compiler and assigned a new identifier if it has not already been
#'   used.
#' @param ... Function arguments. These will be coerced to a substrait.Expression
#'   if they have not been already. Translation functions should take care to
#'   handle R objects that do not readily translate into substrait types
#'   via `as_substrait_expression(x)` before passing them to
#'   `substrait_call()` or `substrait_call_agg()`.
#' @param .output_type The output type of the call. In the future this may
#'   be built in to the compiler since in theory the compiler should be able
#'   to predict this.
#' @param .options An optional list of substrait.FunctionOption messages
#'   to associate with this call.
#' @param expr An expression to evaluate with the translations defined by
#'   the current compiler. You can use this to define translations that use
#'   other translations in a more readable way. You can use tidy evaluation
#'   within `expr`, including unquoting (`!!`), `.data$some_column`, to access
#'   a column explicitly, and `.fns$some_function()` to access a translation
#'   explicitly.
#'
#' @return All of these functions return an object that can be coerced
#'   to a substrait.Expression via `as_substrait_expression(x)`.
#' @export
#'
#' @examples
#' # create a compiler
#' compiler <- substrait_compiler(data.frame(a = 1, b = 2))
#'
#' # usually functions are defined in internal package code,
#' # but you can also define them for testing like this:
#' compiler$.fns$sqrt <- function(x) {
#'   substrait_call("sqrt", x, .output_type = substrait_fp64())
#' }
#'
#' # substrait_eval() does not have access to column names
#' try(with_compiler(compiler, substrait_eval(b)))
#'
#' # ..but does have access to functions
#' with_compiler(compiler, substrait_eval(sqrt(4)))
#'
#' # use substrait_eval_data() to do a more direct test of a translation
#' with_compiler(compiler, substrait_eval_data(sqrt(b)))
#'
substrait_call <- function(.fun, ..., .output_type = NULL, .options = NULL) {
  args <- rlang::list2(...)
  compiler <- current_compiler()
  template <- substrait$Expression$ScalarFunction$create()
  compiler$resolve_function(.fun, args, template, .output_type, .options)
}

#' @rdname substrait_call
#'
#' @param .phase Describes the input type of the data describing what portion of the operation is required
#' @param .invocation Whether the function uses all or only distinct values in the aggregation calculation
#'
#' @seealso [Substrait docs on aggregate function properties](https://substrait.io/expressions/aggregate_functions/#aggregate-binding)
#'
#' @export
substrait_call_agg <- function(.fun, ..., .output_type = NULL, .phase = 0L, .invocation = 0L) {
  args <- rlang::list2(...)
  compiler <- current_compiler()
  template <- substrait$AggregateFunction$create(phase = .phase, invocation = .invocation)
  compiler$resolve_function(.fun, args, template, .output_type)
}

#' @rdname substrait_call
#' @export
substrait_eval <- function(expr) {
  substrait_eval_quo(rlang::enquo(expr), .data = FALSE)
}

#' @rdname substrait_call
#' @export
substrait_eval_data <- function(expr) {
  substrait_eval_quo(rlang::enquo(expr), .data = TRUE)
}

substrait_eval_quo <- function(expr_quo, .data = TRUE) {
  compiler <- current_compiler()
  rlang::eval_tidy(
    expr_quo,
    data = if (is.null(compiler)) NULL else compiler$eval_mask(.data)
  )
}

#' Compiler context
#'
#' When translation functions are called, the compiler that is evaluating
#' them is made available as a global variable accessible via
#' [current_compiler()]. Typically this is not needed directly within a
#' translation function but is used by [substrait_call()] and other functions
#' that need to interact with the current compiler. Translation authors
#' may find [with_compiler()] and/or [local_compiler()] useful to test
#' translation functions directly.
#'
#' @param compiler A [substrait_compiler()] or object that can be
#'   coerced to one.
#' @param code An expression to evaluate with `compiler` as the
#'   [current_compiler()].
#' @param .local_envir The environment for which `compiler` should remain the
#'   [current_compiler()]. This should usually stay the default value of
#'   the calling environment.
#'
#' @return
#'   - [current_compiler()] returns a [substrait_compiler()] or `NULL` if
#'     none has been registered.
#'   - [with_compiler()] returns the result of evaluating `code`
#'   - [local_compiler()] returns the result of coercing `compiler` to a
#'     [substrait_compiler()].
#' @export
#'
#' @examples
#' current_compiler()
#'
#' with_compiler(data.frame(a = 1L), {
#'   current_compiler()$schema
#' })
#'
#' local({
#'   compiler <- local_compiler(data.frame(a = 1L))
#'   compiler$schema
#'   current_compiler()$schema
#' })
#'
#' current_compiler()
#'
current_compiler <- function() {
  compiler_context_env$compiler
}

#' @rdname current_compiler
#' @export
with_compiler <- function(compiler, code) {
  prev_compiler <- current_compiler()
  on.exit(compiler_context_env$compiler <- prev_compiler)
  compiler_context_env$compiler <- substrait_compiler(compiler)
  force(code)
}

#' @rdname current_compiler
#' @export
local_compiler <- function(compiler, .local_envir = parent.frame()) {
  prev_compiler <- current_compiler()
  cleanup <- function() {
    compiler_context_env$compiler <- prev_compiler
  }
  cleanup_call <- rlang::call2(cleanup)
  do.call(base::on.exit, list(cleanup_call, TRUE), envir = .local_envir)
  compiler_context_env$compiler <- substrait_compiler(compiler)
}

compiler_context_env <- new.env(parent = emptyenv())
compiler_context_env$compiler <- NULL

#' @importFrom utils head
#' @export
head.SubstraitCompiler <- function(x, n = 6L, ...) {
  substrait_fetch(x, count = n)
}

substrait_funs <- new.env(parent = emptyenv())

substrait_funs[["%in%"]] <- function(lhs, rhs) {
  lhs <- as_substrait_expression(lhs)
  rhs <- as_substrait_expression(rhs)

  # if the rhs is a regular literal, wrap in a list
  rhs_is_list <- inherits(rhs$literal$list, "substrait_Expression_Literal_List")

  if (!rhs_is_list && inherits(rhs$literal, "substrait_Expression_Literal")) {
    rhs <- substrait_expression_literal_list(rhs$literal)
  } else if (!rhs_is_list) {
    rlang::abort("rhs of %in% must be a list literal (e.g., created using `c()`")
  }

  if (length(rhs$literal$list$values) == 0) {
    return(as_substrait_expression(FALSE))
  } else if (length(rhs$literal$list$values) == 1) {
    return(substrait_eval(lhs == rhs$literal$list$values[[1]]))
  }

  equal_expressions <- lapply(rhs$literal$list$values, function(value) {
    substrait_eval(lhs == value)
  })

  combine_or <- function(lhs, rhs) {
    substrait_eval(lhs | rhs)
  }

  Reduce(combine_or, equal_expressions)
}

substrait_funs[["c"]] <- function(...) {
  args <- rlang::list2(...)
  substrait_expression_literal_list

  substrait$Expression$create(
    literal = substrait$Expression$Literal$create(
      list = substrait$Expression$Literal$List$create(
        values = lapply(args, as_substrait, "substrait.Expression.Literal")
      )
    )
  )
}

substrait_funs[["between"]] <- function(x, left, right) {
  substrait_eval(x >= left & x <= right)
}

substrait_expression_literal_list <- function(values) {
  substrait$Expression$create(
    literal = substrait$Expression$Literal$create(
      list = substrait$Expression$Literal$List$create(
        values = list(values)
      )
    )
  )
}
