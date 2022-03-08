
#' Evaluate a Plan using Arrow
#'
#' @param plan A substrait.Plan
#' @param tables A named list of data frames, Datasets, or Tables
#'   corresponding to NamedTable objects.
#'
#' @return An [arrow::Table].
#' @export
#'
substrait_eval_arrow <- function(plan, tables) {
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

  col_names <- substrait_colnames(plan$relations[[1]])

  # write parquet files
  Map(arrow::write_parquet, tables, temp_parquet)

  # run the exec plan
  getNamespace("arrow")[["do_exec_plan_substrait"]](as.raw(plan), col_names)
}

has_arrow_with_substrait <- function() {
  requireNamespace("arrow", quietly = TRUE) &&
    "do_exec_plan_substrait" %in% names(getNamespace("arrow"))
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
