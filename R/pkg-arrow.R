
ArrowSubstraitCompiler <- R6::R6Class(
  "ArrowSubstraitCompiler", inherit = SubstraitCompiler,
  public = list(
    resolve_function = function(name, args, template) {
      # To get started, just replace the name of the function with the
      # arrow compute name for functions that don't have a custom
      # translation.
      unary_map <- asNamespace("arrow")$.unary_function_map
      binary_map <- asNamespace("arrow")$.binary_function_map

      name <- gsub("^.*?::", "", name)
      if (name %in% names(unary_map)) {
        if (length(args) != 1) {
          stop(
            sprintf(
              "Expected one argument in call to %s (%s) but got %d",
              name, unname(unary_map[name]), length(args)
            )
          )
        }

        name <- unname(unary_map[[name]])
      } else if (name %in% names(binary_map)) {
        if (length(args) != 2) {
          stop(
            sprintf(
              "Expected two arguments in call to %s (%s) but got %d",
              name, unname(unary_map[name]), length(args)
            )
          )
        }

        name <- unname(binary_map[[name]])
      } else {
        stop(
          sprintf(
            "Don't know how to convert call to `%s` to Arrow",
            name
          )
        )
      }

      super$resolve_function(name, args, template)
    },

    evaluate = function(...) {
      plan <- self$plan()

      plan$extension_uris[[1]]$uri <-
        "https://github.com/apache/arrow/blob/master/format/substrait/extension_types.yaml"

      substrait_eval_arrow(
        plan = plan,
        tables = self$named_table_list(),
        col_names = self$schema$names
      )
    }
  )
)

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
  switch(
    .qualified_name,
    "substrait.Type" = {
      type_name <- names(arrow::Type)[x$id + 1L]
      switch(
        type_name,
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

  switch(
    .qualified_name,
    "substrait.Type" = {
      type <- names(msg)
      if (length(type) == 0) {
        return(arrow::null())
      }

      arrow_type_guessed <- switch(
        type,
        "bool_" = arrow::bool(),
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
  switch(
    .qualified_name,
    "substrait.NamedStruct" = {
      types <- lapply(
        x$names,
        function(col) as_substrait(x$GetFieldByName(col), "substrait.Type")
      )

      substrait$NamedStruct$create(
        names = x$names,
        struct_ = substrait$Type$Struct$create(
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
  switch(
    .qualified_name,
    "substrait.NamedStruct" = as_substrait(x$schema, .ptype, ...),
    NextMethod()
  )
}

#' @export
from_substrait.Schema <- function(msg, x, ...) {
  .qualified_name <- make_qualified_name(msg)

  switch(
    .qualified_name,
    "substrait.NamedStruct" = {
      if (length(x) == 0) {
        ptype <- rep_len(list(arrow::null()), length(msg$names))
        names(ptype) <- msg$names
      } else {
        ptype <- lapply(x$names, function(col) x$GetFieldByName(col)$type)
        names(ptype) <- x$names
      }

      stopifnot(identical(names(ptype), msg$names))
      ptype <- Map(from_substrait, msg$struct_$types, ptype)
      names(ptype) <- msg$names
      arrow::schema(!!! ptype)
    },
    NextMethod()
  )
}

#' @export
from_substrait.RecordBatch <- function(msg, x, ...) {
  .qualified_name <- make_qualified_name(msg)

  switch(
    .qualified_name,
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
  plan <- as_substrait(plan, "substrait.Plan")
  stopifnot(rlang::is_named2(tables))

  # only support plans with exactly one relation in the relations list for now
  stopifnot(length(plan$relations) == 1)

  temp_parquet <- vapply(tables, function(i) tempfile(), character(1))
  on.exit(unlink(temp_parquet))

  local_file_tables <- lapply(seq_along(tables), function(i) {
    substrait$ReadRel$LocalFiles$create(
      items = list(
        substrait$ReadRel$LocalFiles$FileOrFiles$create(
          uri_file = sprintf("file://%s", temp_parquet[i]),
          format = substrait$ReadRel$LocalFiles$FileOrFiles$FileFormat$
            FILE_FORMAT_PARQUET
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
  names(result) <- col_names
  result
}

has_arrow_with_substrait <- function() {
  requireNamespace("arrow", quietly = TRUE) &&
    "do_exec_plan_substrait" %in% names(getNamespace("arrow")) &&
    length(formals(getNamespace("arrow")$do_exec_plan_substrait)) == 1
}
