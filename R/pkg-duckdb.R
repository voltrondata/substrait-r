
duckdb_get_substrait <- function(sql) {
  ensure_custom_duckdb()
  # see
  # https://github.com/duckdb/duckdb/blob/0b9c1c20eccf1a65a5556bd30d8817318e6b850f/test/sql/substrait/test_substrait.test#L10-L19
}

# basically the same as arrow...write temporary parquet files and shove the plan
# into duckdb using CALL from_substrait()
substrait_eval_duckdb <- function(plan, tables, col_names) {
  # see
  # https://github.com/duckdb/duckdb/blob/8264eea013069a9a3ee3075e189c1f92c7cca5a1/test/sql/substrait/test_substrait.test#L23

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

  # ...call duckdb
}

ensure_custom_duckdb <- function(lib = custom_duckdb_lib_dir(), install = TRUE,
                                 quiet = FALSE) {
  result <- tryCatch({
    query_custom_duckdb(
      "select scale_factor, query_nr from tpch_answers() LIMIT 1;",
      lib = lib
    )
  },
  error = function(e) {
    error_is_from_us <- grepl(
      "(name tpch_answers does not exist)|(there is no package called 'duckdb')",
      conditionMessage(e)
    )

    if (error_is_from_us) {
      NULL
    } else {
      rlang::abort(
        "An unexpected error occured whilst querying TPC-H enabled duckdb",
        parent = e
      )
    }
  }
  )

  # Check that the result has a query in it
  if (identical(result$query_nr, 1L)) {
    return(invisible(NULL))
  }

  if (install) {
    install_custom_duckdb(lib, quiet = quiet)
    result <- try(
      ensure_custom_duckdb(lib, install = FALSE, quiet = quiet),
      silent = TRUE
    )

    if (!inherits(result, "try-error")) {
      return(invisible(NULL))
    }
  }

  stop(
    paste(
      "Custom duckdb build with SUBSTRAIT extension could not be loaded",
      if (install) "and could not be installed." else "and `install = FALSE`"
    )
  )
}

query_custom_duckdb <- function(sql, dbdir = ":memory:", lib = custom_duckdb_lib_dir()) {
  fun <- function(sql, dbdir, lib) {
    # don't load duckdb from anything except `lib` and error otherwise
    # because the subprocess may have duckdb in a default, site, or user lib
    if (!requireNamespace("duckdb", lib.loc = lib, quietly = TRUE)) {
      stop(
        sprintf("there is no package called 'duckdb'"),
        call. = FALSE
      )
    }

    con <- DBI::dbConnect(duckdb::duckdb(dbdir = dbdir))
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    DBI::dbGetQuery(con, sql)
  }

  callr::r(fun, list(sql, dbdir, lib), libpath = c(lib, .libPaths()))
}

export_custom_duckdb <- function(sql, sink, dbdir = ":memory:", lib = custom_duckdb_lib_dir()) {
  fun <- function(sql, sink, dbdir, lib) {
    # don't load duckdb from anything except `lib` and error otherwise
    # because the subprocess may have duckdb in a default, site, or user lib
    if (!requireNamespace("duckdb", lib.loc = lib, quietly = TRUE)) {
      stop(
        sprintf("there is no package called 'duckdb'"),
        call. = FALSE
      )
    }

    con <- DBI::dbConnect(duckdb::duckdb(dbdir = dbdir))
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    res <- DBI::dbSendQuery(con, sql, arrow = TRUE)

    # this could be streamed in the future when the parquet writer
    # in R supports streaming
    reader <- duckdb::duckdb_fetch_record_batch(res)
    table <- reader$read_table()
    arrow::write_parquet(table, sink)
    sink
  }

  callr::r(fun, list(sql, sink, dbdir, lib), libpath = c(lib, .libPaths()))
}

install_custom_duckdb <- function(lib = custom_duckdb_lib_dir(), force = TRUE, quiet = FALSE) {
  if (!quiet) {
    message(
      paste0(
        "Installing duckdb with the ability to run substrait ",
        "to custom library \n'", lib, "'"
      )
    )
  }

  # `build = FALSE` so that the duckdb cpp source is available when the R package
  # is compiling itself
  fun <- function(lib) {
    if (!dir.exists(lib)) {
      dir.create(lib, recursive = TRUE)
    }

    remotes::install_cran("DBI", lib = lib, force = force)
    remotes::install_github(
      "duckdb/duckdb/tools/rpkg",
      build = FALSE,
      force = force,
      lib = lib
    )
  }

  withr::with_envvar(
    list(DUCKDB_R_EXTENSIONS = "substrait"),
    callr::r(fun, list(lib), libpath = c(lib, .libPaths()), show = !quiet)
  )
}

custom_duckdb_lib_dir <- function() {
  file.path(rappdirs::user_data_dir("R-substrait"), "duckdb_lib")
}
