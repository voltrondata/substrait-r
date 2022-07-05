
#' DuckDB Substrait Interface
#'
#' @param sql An SQL expression from which to generate a Substrait plan
#' @param plan A substrait.Plan proto object
#' @param as_data_frame Use `FALSE` to return an [arrow::Table] instead of a
#'   data.frame.
#' @param tables A named list of tables to populate the database
#' @param col_names The final column names for the result
#' @param lib A directry where the custom duckdb will be installed
#' @param force,quiet Passed to the remotes installer
#'
#' @return
#'   - `duckdb_get_substrait()`: a substrait.Plan protobuf object
#'   - `duckdb_from_substrait()`: A data.frame or arrow Table
#' @export
#'
#' @examplesIf has_duckdb_with_substrait()
#' plan <- duckdb_get_substrait(
#'   "SELECT * from mtcars WHERE mpg > 30",
#'   tables = list(mtcars = mtcars)
#' )
#'
#' duckdb_from_substrait(plan, tables = list(mtcars = mtcars))
#'
duckdb_get_substrait <- function(sql, tables = list()) {
  stopifnot(has_duckdb_with_substrait())

  temp_con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(temp_con, shutdown = TRUE))
  sql_quoted <- DBI::dbQuoteLiteral(temp_con, sql)

  result <- query_duckdb_with_substrait(
    sprintf("CALL get_substrait(%s)", sql_quoted),
    tables = tables,
    as_data_frame = TRUE
  )
  plan <- unclass(result[[1]])[[1]]
  ptype <- make_ptype("substrait.Plan")
  structure(
    list(content = plan),
    class = class(ptype)
  )
}


#' @rdname duckdb_get_substrait
#' @export
duckdb_from_substrait <- function(plan, tables = list(),
                                  col_names = plan$relations[[1]]$root$names,
                                  as_data_frame = TRUE) {
  stopifnot(has_duckdb_with_substrait())

  plan <- as_substrait(plan, "substrait.Plan")

  result <- query_duckdb_with_substrait(
    sprintf("CALL from_substrait(%s)", duckdb_encode_blob(plan)),
    as_data_frame = as_data_frame,
    tables = tables
  )

  names(result) <- col_names
  result
}

#' @rdname duckdb_get_substrait
#' @export
has_duckdb_with_substrait <- function(lib = duckdb_with_substrait_lib_dir()) {
  if (!identical(duckdb_works_cache$works, NA)) {
    return(duckdb_works_cache$works)
  }

  # we also need arrow for this, so check that here
  if (!requireNamespace("arrow", quietly = TRUE)) {
    return(FALSE)
  }

  duckdb_works_cache$works <- tryCatch({
    query_duckdb_with_substrait(
      query_duckdb_with_substrait("CALL from_substrait()"),
      lib = lib
    )
    TRUE
  },
  error = function(e) {
    from_substrait_exists <- grepl(
      "from_substrait\\(BLOB\\)",
      conditionMessage(e)
    )

    error_is_from_us <- grepl(
      "there is no package called 'duckdb'",
      conditionMessage(e)
    )

    if (from_substrait_exists) {
      TRUE
    } else if (error_is_from_us) {
      FALSE
    } else {
      rlang::abort(
        "An unexpected error occured whilst querying Substrait-enabled duckdb",
        parent = e
      )
    }
  })

  duckdb_works_cache$works
}

query_duckdb_with_substrait <- function(sql, dbdir = ":memory:",
                                        lib = duckdb_with_substrait_lib_dir(),
                                        tables = list(),
                                        as_data_frame = TRUE) {
  sink <- tempfile()

  # write all tables to temporary parquet files so we can load them in to
  # duckdb from another process
  stopifnot(rlang::is_named2(tables))

  temp_parquet <- vapply(tables, function(i) tempfile(), character(1))
  on.exit(unlink(c(sink, temp_parquet)))
  for (i in seq_along(tables)) {
    arrow::write_parquet(tables[[i]], temp_parquet[i]);
  }

  fun <- function(sql, sink, dbdir, lib, temp_parquet) {
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

    # register all the temporary parquet files as named tables
    for (i in seq_along(temp_parquet)) {
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TABLE %s AS SELECT * FROM parquet_scan(%s);",
          DBI::dbQuoteIdentifier(con, names(temp_parquet)[i]),
          DBI::dbQuoteLiteral(con, temp_parquet[i])
        )
      )
    }

    res <- DBI::dbSendQuery(con, sql, arrow = TRUE)

    # this could be streamed in the future when the parquet writer
    # in R supports streaming
    reader <- duckdb::duckdb_fetch_record_batch(res)
    table <- reader$read_table()
    arrow::write_parquet(table, sink)
    sink
  }

  callr::r(
    fun,
    list(sql, sink, dbdir, lib, temp_parquet),
    libpath = c(lib, .libPaths())
  )

  arrow::read_parquet(sink, as_data_frame = as_data_frame)
}

#' @rdname duckdb_get_substrait
#' @export
install_duckdb_with_substrait <- function(lib = duckdb_with_substrait_lib_dir(),
                                          force = TRUE, quiet = FALSE) {
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

  duckdb_works_cache$works <- NA
}

#' @rdname duckdb_get_substrait
#' @export
duckdb_with_substrait_lib_dir <- function() {
  Sys.getenv(
    "R_SUBSTRAIT_DUCKDB_LIB",
    file.path(rappdirs::user_data_dir("R-substrait"), "duckdb_lib")
  )
}

duckdb_encode_blob <- function(x) {
  data <- paste0("\\x", as.raw(x), collapse = "")
  paste0("'", data, "'::BLOB")
}

duckdb_works_cache <- new.env(parent = emptyenv())
duckdb_works_cache$works <- NA



#' Create an DuckDB Substrait Compiler
#'
#' @inheritParams arrow_substrait_compiler
#'
#' @return A [SubstraitCompiler] subclass.
#' @export
#'
duckdb_substrait_compiler <- function(object, ...) {
  DuckDBSubstraitCompiler$new(object, ...)
}

DuckDBSubstraitCompiler <- R6::R6Class(
  "DuckDBSubstraitCompiler", inherit = SubstraitCompiler,
  public = list(
    resolve_function = function(name, args, template) {
      # Note that this is a quick-and-dirty implementation designed to help
      # test the core functionality with realistic tests. The fact that this
      # is pretty ugly suggests that expression translation should maybe live
      # in the compiler rather than its current form (functions in
      # expressions.R).

      # skip package names for now
      name <- gsub("^.*?::", "", name)

      # Note: super$resolve_function() will skip any custom things we do here,
      # whereas self$resolve_function() will apply translations as we
      # implement them here.
      switch(
        name,
        "==" = super$resolve_function("equal", args, template),
        "!=" = super$resolve_function("not_equal", args, template),
        ">=" = super$resolve_function("gte", args, template),
        "<=" = super$resolve_function("lte", args, template),
        ">" = super$resolve_function("gt", args, template),
        "<" = super$resolve_function("lt", args, template),
        "between" = super$resolve_function(
          "and",
          list(
            super$resolve_function("gte", args[-3], template),
            super$resolve_function("lte", args[-2], template)
          ),
          template
        ),
        "&" = super$resolve_function("and", args, template),
        "|" = super$resolve_function("or", args, template),
        # while I'm sure that "not" exists somehow, this is the only way
        # I can get it to work for now (NULLs are not handled properly here)
        "!" = {
          substrait$Expression$create(
            cast = substrait$Expression$Cast$create(
              type = substrait$Type$create(
                bool_ = substrait$Type$Boolean$create()
              ),
              input = substrait$Expression$create(
                if_then = substrait$Expression$IfThen$create(
                  ifs = list(
                    substrait$Expression$IfThen$IfClause$create(
                      if_ = as_substrait(
                        args[[1]],
                        "substrait.Expression",
                        compiler = compiler
                      ),
                      then = as_substrait(FALSE, "substrait.Expression")
                    )
                  ),
                  else_ = as_substrait(TRUE, "substrait.Expression")
                )
              )
            )
          )
        },
        "is.na" = {
          self$resolve_function(
            "!",
            list(
              as_substrait(
                super$resolve_function("is_not_null", args, template),
                "substrait.Expression"
              )
            ),
            template
          )
        },
        "c" = {
          # this limits the usage of c() to literals, which is probably the most
          # common usage (e.g., col %in% c("a", "b"))
          substrait$Expression$create(
            literal = substrait$Expression$Literal$create(
              list = substrait$Expression$Literal$List$create(
                values = lapply(args, as_substrait, "substrait.Expression.Literal")
              )
            )
          )
        },
        "%in%" = {
          # duckdb implements this using == and or, according to
          # duckdb_get_substrait()
          lhs <- as_substrait(args[[1]], "substrait.Expression", compiler = self)
          rhs <- as_substrait(args[[2]], "substrait.Expression", compiler = self)

          # if the rhs is a regular literal, wrap in a list
          rhs_is_list <- inherits(rhs$literal$list, "substrait_Expression_Literal_List")

          if (!rhs_is_list && inherits(rhs$literal, "substrait_Expression_Literal")) {
            rhs <- substrait$Expression$create(
              literal = substrait$Expression$Literal$create(
                list = substrait$Expression$Literal$List$create(
                  values = list(rhs$literal)
                )
              )
            )
          } else if (!rhs_is_list) {
            rlang::abort("rhs of %in% must be a list literal (e.g., created using `c()`")
          }

          if (length(rhs$literal$list$values) == 0) {
            return(as_substrait(FALSE, "substrait.Expression"))
          } else if (length(rhs$literal$list$values) == 1) {
            return(
              super$resolve_function(
                "equal",
                list(lhs, rhs$literal$list$values[[1]]),
                template
              )
            )
          }

          equal_expressions <- lapply(rhs$literal$list$values, function(value) {
            as_substrait(
              super$resolve_function("equal", list(lhs, value), template),
              "substrait.Expression"
            )
          })

          combine_or <- function(lhs, rhs) {
            as_substrait(
              super$resolve_function("or", list(lhs, rhs), template),
              "substrait.Expression"
            )
          }

          Reduce(combine_or, equal_expressions)
        },

        # pass through by default (but listing here the functions that are
        # known to work with the same names as R)
        "+" = , "-" = , "*" = , "/" = , "^" = ,
        super$resolve_function(name, args, template)
      )
    },
    validate = function(){
      super$validate()
      # DuckDB backend doesn't accept empty SELECT clause
      if (length(self$schema$names) == 0) {
        rlang::abort("Column list must not be empty")
      }
      self
    },
    evaluate = function(...) {
      duckdb_from_substrait(
        plan = self$plan(),
        tables = self$named_table_list(),
        as_data_frame = FALSE
      )
    }
  )
)
