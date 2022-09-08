
#' DuckDB Substrait Interface
#'
#' @param sql An SQL expression from which to generate a Substrait plan
#' @param plan A substrait.Plan proto object
#' @param as_data_frame Use `FALSE` to return an [arrow::Table] instead of a
#'   data.frame.
#' @param tables A named list of tables to populate the database
#' @param col_names The final column names for the result
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

  result <- with_duckdb_tables(tables, function(con) {
    duckdb::duckdb_get_substrait(con, sql)
  })

  structure(
    list(content = result),
    class = class(make_ptype("substrait.Plan"))
  )
}


#' @rdname duckdb_get_substrait
#' @export
duckdb_from_substrait <- function(plan, tables = list(),
                                  col_names = plan$relations[[1]]$root$names,
                                  as_data_frame = TRUE) {
  stopifnot(has_duckdb_with_substrait())

  plan <- as_substrait(plan, "substrait.Plan")
  result <- with_duckdb_tables(tables, function(con) {
    res <- duckdb::duckdb_prepare_substrait(con, unclass(plan)$content, arrow = TRUE)
    reader <- duckdb::duckdb_fetch_record_batch(res)
    if (as_data_frame) {
      tibble::as_tibble(as.data.frame(reader$read_table()))
    } else {
      reader$read_table()
    }
  })

  names(result) <- col_names
  result
}

#' @rdname duckdb_get_substrait
#' @export
has_duckdb_with_substrait <- function() {
  if (!identical(duckdb_works_cache$works, NA)) {
    return(duckdb_works_cache$works)
  }

  # we also need arrow for this, so check that here
  if (!requireNamespace("arrow", quietly = TRUE)) {
    return(FALSE)
  }

  duckdb_works_cache$works <- tryCatch(
    with_duckdb_tables(list(), function(con) TRUE),
    error = function(e) FALSE
  )

  duckdb_works_cache$works
}

# The check takes enough time that we cache the result after the first check.
duckdb_works_cache <- new.env(parent = emptyenv())
duckdb_works_cache$works <- NA

with_duckdb_tables <- function(tables, fun) {
  # Write all tables to temporary parquet files so we can load them in to
  # duckdb (TODO: don't use files and/or use VIEW. A VIEW currently
  # does not work with duckdb's substrait preparation).
  stopifnot(rlang::is_named2(tables))

  con <- DBI::dbConnect(duckdb::duckdb())
  temp_parquet <- vapply(tables, function(i) tempfile(), character(1))
  on.exit({
    unlink(temp_parquet)
    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  DBI::dbExecute(con, "INSTALL substrait; LOAD substrait;")

  for (i in seq_along(tables)) {
    arrow::write_parquet(tables[[i]], temp_parquet[i])
  }

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

  fun(con)
}

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
  "DuckDBSubstraitCompiler",
  inherit = SubstraitCompiler,
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
      switch(name,
        "==" = super$resolve_function("equal", args, template, output_type = substrait_boolean()),
        "!=" = super$resolve_function("not_equal", args, template, output_type = substrait_boolean()),
        ">=" = super$resolve_function("gte", args, template, output_type = substrait_boolean()),
        "<=" = super$resolve_function("lte", args, template, output_type = substrait_boolean()),
        ">" = super$resolve_function("gt", args, template, output_type = substrait_boolean()),
        "<" = super$resolve_function("lt", args, template, output_type = substrait_boolean()),
        "between" = super$resolve_function(
          "and",
          list(
            super$resolve_function("gte", args[-3], template, output_type = substrait_boolean()),
            super$resolve_function("lte", args[-2], template, output_type = substrait_boolean())
          ),
          template,
          output_type = substrait_boolean()
        ),
        "&" = super$resolve_function("and", args, template, output_type = substrait_boolean()),
        "|" = super$resolve_function("or", args, template, output_type = substrait_boolean()),
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
                super$resolve_function("is_not_null", args, template, output_type = substrait_boolean()),
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
                template,
                output_type = substrait_boolean()
              )
            )
          }

          equal_expressions <- lapply(rhs$literal$list$values, function(value) {
            as_substrait(
              super$resolve_function("equal", list(lhs, value), template, output_type = substrait_boolean()),
              "substrait.Expression"
            )
          })

          combine_or <- function(lhs, rhs) {
            as_substrait(
              super$resolve_function("or", list(lhs, rhs), template, output_type = substrait_boolean()),
              "substrait.Expression"
            )
          }

          Reduce(combine_or, equal_expressions)
        },

        # pass through by default (but listing here the functions that are
        # known to work with the same names as R)
        "+" = ,
        "-" = ,
        "*" = ,
        "/" = ,
        "^" = ,
        "sum" = super$resolve_function(name, args, template, output_type = function(x, y) x),
        rlang::abort(
          paste0('could not find function "', name, '"')
        )
      )
    },
    validate = function() {
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
