
ArrowSubstraitCompiler <- R6::R6Class(
  "ArrowSubstraitCompiler",
  inherit = SubstraitCompiler,
  private = list(extension_uri = NULL),
  public = list(
    initialize = function(...) {
      super$initialize(...)
      self$.fns <- arrow_funs
      private$extension_uri <- list(
        "arithmetic" = substrait$extensions$SimpleExtensionURI$create(
          extension_uri_anchor = 1L,
          uri = "https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml"
        ),
        "comparison" = substrait$extensions$SimpleExtensionURI$create(
          extension_uri_anchor = 2L,
          uri = "https://github.com/substrait-io/substrait/blob/main/extensions/functions_comparison.yaml"
        )
      )
    },
    extension_uri_anchor = function(name) {
      prefix <- strsplit(name, ".", fixed = TRUE)[[1]][1]
      private$extension_uri[[prefix]]$extension_uri_anchor
    },
    evaluate = function(...) {
      plan <- self$plan()

      substrait_eval_arrow(
        plan = plan,
        tables = self$named_table_list(),
        col_names = self$schema$names
      )
    },
    plan = function() {
      plan <- super$plan()

      for (i in seq_along(plan$extensions)) {
        if (is.null(plan$extensions[[i]]$extension_function)) {
          next
        }

        short_name <- strsplit(plan$extensions[[i]]$extension_function$name, ".", fixed = TRUE)[[1]][2]
        plan$extensions[[i]]$extension_function$name <- short_name
      }

      plan
    }
  )
)

# Scalar functions
arrow_funs <- new.env(parent = emptyenv())

arrow_funs[["+"]] <- function(lhs, rhs) {
  substrait_call(
    "arithmetic.add",
    lhs,
    rhs,
    .output_type = function(lhs, rhs) rhs,
    .options = list(
      substrait$FunctionOption$create(
        name = "overflow",
        preference = "ERROR"
      )
    )
  )
}

# TODO: remove non-default `.phase` and `.invocation` param values for aggregation functions when Arrow consumer supports this

arrow_funs[["sum"]] <- function(x, na.rm = FALSE) {
  check_na_rm(na.rm)
  substrait_call_agg("arithmetic.sum", x, .output_type = substrait_fp64(), .phase = 3L, .invocation = 1L)
}

arrow_funs[["mean"]] <- function(x, na.rm = FALSE) {
  check_na_rm(na.rm)
  substrait_call_agg("arithmetic.avg", x, .output_type = substrait_fp64(), .phase = 3L, .invocation = 1L)
}

arrow_funs[["min"]] <- function(x, na.rm = FALSE) {
  check_na_rm(na.rm)
  substrait_call_agg("arithmetic.min", x, .output_type = substrait_fp64(), .phase = 3L, .invocation = 1L)
}

arrow_funs[["max"]] <- function(x, na.rm = FALSE) {
  check_na_rm(na.rm)
  substrait_call_agg("arithmetic.max", x, .output_type = substrait_i64(), .phase = 3L, .invocation = 1L)
}

# Comparison functions
arrow_funs[["!="]] <- function(lhs, rhs) {
  substrait_call(
    "comparison.not_equal",
    lhs,
    rhs,
    .output_type = substrait_boolean()
  )
}

arrow_funs[["=="]] <- function(lhs, rhs) {
  substrait_call(
    "comparison.equal",
    lhs,
    rhs,
    .output_type = substrait_boolean()
  )
}

arrow_funs[["<"]] <- function(lhs, rhs) {
  substrait_call(
    "comparison.lt",
    lhs,
    rhs,
    .output_type = substrait_boolean()
  )
}

arrow_funs[[">"]] <- function(lhs, rhs) {
  substrait_call(
    "comparison.gt",
    lhs,
    rhs,
    .output_type = substrait_boolean()
  )
}

arrow_funs[["<="]] <- function(lhs, rhs) {
  substrait_call(
    "comparison.lte",
    lhs,
    rhs,
    .output_type = substrait_boolean()
  )
}

arrow_funs[[">="]] <- function(lhs, rhs) {
  substrait_call(
    "comparison.gte",
    lhs,
    rhs,
    .output_type = substrait_boolean()
  )
}

check_na_rm <- function(na.rm) {
  if (!na.rm) {
    warning("Missing value removal from aggregate functions not yet supported, switching to na.rm = TRUE")
  }
}

#' Create an Arrow Substrait Compiler
#'
#' @param object A [data.frame()], [arrow::Table], [arrow::RecordBatch],
#'   or [arrow::Dataset], or anything else that can be written to a
#'   parquet file using [arrow::write_parquet()].
#' @param ... Unused.
#'
#' @return A [SubstraitCompiler] subclass that will
#' @export
#'
arrow_substrait_compiler <- function(object, ...) {
  ArrowSubstraitCompiler$new(object, ...)
}

#' @export
substrait_compiler.ArrowTabular <- function(object, ...) {
  arrow_substrait_compiler(object, ...)
}

#' @export
as_substrait.DataType <- function(x, .ptype = NULL, ...) {
  if (is.null(.ptype)) {
    .ptype <- substrait$Type$create()
  }

  .qualified_name <- make_qualified_name(.ptype)
  switch(.qualified_name,
    "substrait.Type" = {
      type_name <- names(arrow::Type)[x$id + 1L]
      switch(type_name,
        "BOOL" = substrait_boolean(),
        "INT32" = substrait_i32(),
        "DOUBLE" = substrait_fp64(),
        "STRING" = substrait_string(),
        stop(sprintf("Can't map Arrow DataType '%s' to substrait.Type", x$ToString()))
      )
    },
    NextMethod()
  )
}

#' @export
from_substrait.DataType <- function(msg, x, ...) {
  .qualified_name <- make_qualified_name(msg)

  switch(.qualified_name,
    "substrait.Type" = {
      type <- names(msg)
      if (length(type) == 0) {
        return(arrow::null())
      }

      arrow_type_guessed <- switch(type,
        "bool" = arrow::bool(),
        "i32" = arrow::int32(),
        "fp64" = arrow::float64(),
        "string" = arrow::string(),
        stop(sprintf("Can't convert substrait.Type<%s> to arrow DataType", type))
      )

      if (x != arrow::null()) {
        stopifnot(arrow_type_guessed == x)
      }

      arrow_type_guessed
    },
    NextMethod()
  )
}

#' @export
as_substrait.Field <- function(x, .ptype = NULL, ...) {
  as_substrait(x$type, .ptype = .ptype)
}

#' @export
as_substrait.Schema <- function(x, .ptype = NULL, ...) {
  if (is.null(.ptype)) {
    .ptype <- substrait$NamedStruct$create()
  }

  .qualified_name <- make_qualified_name(.ptype)
  switch(.qualified_name,
    "substrait.NamedStruct" = {
      types <- lapply(
        x$names,
        function(col) as_substrait(x$GetFieldByName(col), "substrait.Type")
      )

      substrait$NamedStruct$create(
        names = x$names,
        struct = substrait$Type$Struct$create(
          types = types
        )
      )
    },
    NextMethod()
  )
}

#' @export
as_substrait.ArrowTabular <- function(x, .ptype = NULL, ...) {
  if (is.null(.ptype)) {
    .ptype <- substrait$NamedStruct$create()
  }

  .qualified_name <- make_qualified_name(.ptype)
  switch(.qualified_name,
    "substrait.NamedStruct" = as_substrait(x$schema, .ptype, ...),
    NextMethod()
  )
}

#' @export
from_substrait.Schema <- function(msg, x, ...) {
  .qualified_name <- make_qualified_name(msg)

  switch(.qualified_name,
    "substrait.NamedStruct" = {
      if (length(x) == 0) {
        ptype <- rep_len(list(arrow::null()), length(msg$names))
        names(ptype) <- msg$names
      } else {
        ptype <- lapply(x$names, function(col) x$GetFieldByName(col)$type)
        names(ptype) <- x$names
      }

      stopifnot(identical(names(ptype), msg$names))
      ptype <- Map(from_substrait, msg$struct$types, ptype)
      names(ptype) <- msg$names
      arrow::schema(!!!ptype)
    },
    NextMethod()
  )
}

#' @export
from_substrait.RecordBatch <- function(msg, x, ...) {
  .qualified_name <- make_qualified_name(msg)

  switch(.qualified_name,
    "substrait.NamedStruct" = {
      schema <- from_substrait(msg, arrow::schema())
      empty <- x[character()]$Take(integer())
      for (col in names(schema)) {
        empty[[col]] <- arrow::Array$create(
          logical(),
          type = arrow::null()
        )$cast(schema[[col]]$type)
      }

      empty
    },
    NextMethod()
  )
}

substrait_eval_arrow <- function(plan, tables, col_names) {
  stopifnot(has_arrow_with_substrait())
  plan <- as_substrait(plan, "substrait.Plan")
  stopifnot(rlang::is_named2(tables))

  # only support plans with exactly one relation in the relations list for now
  stopifnot(length(plan$relations) == 1)

  # arrow uses PlanRel(rel = ) instead of PlanRel(root = RelRoot(input = ))
  if (!is.null(plan$relations[[1]]$root)) {
    plan_rel <- substrait$PlanRel$create(
      rel = plan$relations[[1]]$root$input
    )

    plan$relations[[1]] <- plan_rel
  }

  temp_parquet <- vapply(tables, function(i) tempfile(), character(1))
  on.exit(unlink(temp_parquet))

  local_file_tables <- lapply(seq_along(tables), function(i) {
    substrait$ReadRel$LocalFiles$create(
      items = list(
        substrait$ReadRel$LocalFiles$FileOrFiles$create(
          uri_file = sprintf("file://%s", temp_parquet[i]),
          parquet = substrait$ReadRel$LocalFiles$FileOrFiles$ParquetReadOptions$create()
        )
      )
    )
  })
  names(local_file_tables) <- names(tables)

  table_base_schema <- lapply(tables, as_substrait, "substrait.NamedStruct")

  call_for_errors <- sys.call()

  # walk the relation tree looking for named tables, replacing
  # with those from local_file_tables
  plan <- rel_tree_modify(plan, "substrait_ReadRel", function(x) {
    if (isTRUE("named_table" %in% names(x))) {
      name <- x$named_table$names

      if (!isTRUE(name %in% names(local_file_tables))) {
        rlang::abort(
          sprintf("Named table '%s' not found in `tables`", name),
          call = call_for_errors
        )
      }

      if (!identical(x$base_schema, table_base_schema[[name]])) {
        rlang::abort(
          sprintf(
            "Base schema for table '%s' does not match declared base schema",
            name
          ),
          call = call_for_errors
        )
      }

      x$named_table <- NULL
      x$local_files <- local_file_tables[[name]]
      x
    } else {
      x
    }
  })

  # write parquet files
  Map(arrow::write_parquet, tables, temp_parquet)

  # run the exec plan
  result <- getNamespace("arrow")[["do_exec_plan_substrait"]](as.raw(plan))

  # don't include augmented fields like __fragment_index
  names <- names(result)
  names <- names[!grepl("^__", names)]
  result <- result[names]

  # reassign the column names
  names(result) <- col_names %||% character(0)
  result
}

has_arrow_with_substrait <- function() {
  # ...we need arrow installed
  requireNamespace("arrow", quietly = TRUE) &&
    # ...with do_exec_plan_substrait()
    "do_exec_plan_substrait" %in% names(getNamespace("arrow")) &&
    # ...with the right number of arguments (was modified by a recent PR)
    length(formals(getNamespace("arrow")$do_exec_plan_substrait)) == 1 &&
    # ...and we need it not to be a shell that will error because arrow wasn't
    # built with ARROW_ENGINE=ON
    identical(arrow::arrow_info()$capabilities["substrait"], c("substrait" = TRUE))
}
