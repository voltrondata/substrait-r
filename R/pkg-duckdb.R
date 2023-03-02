
#' DuckDB Substrait Interface
#'
#' @param sql An SQL expression from which to generate a Substrait plan
#' @param plan A substrait.Plan proto object
#' @param as_data_frame Use `FALSE` to return an [arrow::Table] instead of a
#'   data.frame.
#' @param tables A named list of tables to populate the database
#' @param col_names The final column names for the result
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
    error = function(e) {
      message(conditionMessage(e))
      FALSE
    }
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
    initialize = function(...) {
      super$initialize(...)
      self$.fns <- c(as.list(duckdb_funs), as.list(substrait_funs))
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

# Scalar functions
duckdb_funs <- new.env(parent = emptyenv())

duckdb_funs[["=="]] <- function(lhs, rhs) {
  substrait_call("equal", lhs, rhs, .output_type = substrait_boolean())
}

duckdb_funs[["!="]] <- function(lhs, rhs) {
  substrait_call("not_equal", lhs, rhs, .output_type = substrait_boolean())
}

duckdb_funs[[">="]] <- function(lhs, rhs) {
  substrait_call("gte", lhs, rhs, .output_type = substrait_boolean())
}

duckdb_funs[["<="]] <- function(lhs, rhs) {
  substrait_call("lte", lhs, rhs, .output_type = substrait_boolean())
}

duckdb_funs[[">"]] <- function(lhs, rhs) {
  substrait_call("gt", lhs, rhs, .output_type = substrait_boolean())
}

duckdb_funs[["<"]] <- function(lhs, rhs) {
  substrait_call("lt", lhs, rhs, .output_type = substrait_boolean())
}

duckdb_funs[["&"]] <- function(lhs, rhs) {
  substrait_call("and", lhs, rhs, .output_type = substrait_boolean())
}

duckdb_funs[["|"]] <- function(lhs, rhs) {
  substrait_call("or", lhs, rhs, .output_type = substrait_boolean())
}

# While I'm sure that "not" exists somehow, this is the only way
# I can get it to work for now (NULLs are not handled properly here)
duckdb_funs[["!"]] <- function(rhs) {
  substrait$Expression$create(
    cast = substrait$Expression$Cast$create(
      type = substrait$Type$create(
        bool = substrait$Type$Boolean$create()
      ),
      input = substrait$Expression$create(
        if_then = substrait$Expression$IfThen$create(
          ifs = list(
            substrait$Expression$IfThen$IfClause$create(
              `if` = as_substrait_expression(rhs),
              then = as_substrait_expression(FALSE)
            )
          ),
          `else` = as_substrait_expression(TRUE)
        )
      )
    )
  )
}

duckdb_funs[["is.na"]] <- function(x) {
  is_not_null <- substrait_call("is_not_null", x, .output_type = substrait_boolean())
  substrait_eval(!is_not_null)
}

duckdb_funs[["c"]] <- function(...) {
  # this limits the usage of c() to literals, which is probably the most
  # common usage (e.g., col %in% c("a", "b"))
  args <- rlang::list2(...)
  substrait$Expression$create(
    literal = substrait$Expression$Literal$create(
      list = substrait$Expression$Literal$List$create(
        values = lapply(args, as_substrait, "substrait.Expression.Literal")
      )
    )
  )
}

duckdb_funs[["+"]] <- function(lhs, rhs) {
  if (missing(rhs)) {
    substrait_call("+", lhs, .output_type = function(lhs) lhs)
  } else {
    substrait_call("+", lhs, rhs, .output_type = function(lhs, rhs) lhs)
  }
}

duckdb_funs[["-"]] <- function(lhs, rhs) {
  if (missing(rhs)) {
    substrait_call("-", lhs, .output_type = function(lhs) lhs)
  } else {
    substrait_call("-", lhs, rhs, .output_type = function(lhs, rhs) lhs)
  }
}

duckdb_funs[["*"]] <- function(lhs, rhs) {
  substrait_call("*", lhs, rhs, .output_type = function(lhs, rhs) lhs)
}

duckdb_funs[["/"]] <- function(lhs, rhs) {
  substrait_call("/", lhs, rhs, .output_type = function(lhs, rhs) lhs)
}

duckdb_funs[["^"]] <- function(lhs, rhs) {
  substrait_call("^", lhs, rhs, .output_type = function(lhs, rhs) lhs)
}

duckdb_funs[["sqrt"]] <- function(x) {
  substrait_call(
    "sqrt",
    x,
    .output_type = substrait_fp64(),
    .options = list(
      substrait$FunctionOption$create(
        name = "overflow",
        preference = "SILENT"
      )
    )
  )
}

duckdb_funs[["abs"]] <- function(x) {
  substrait_call(
    "abs",
    x,
    .output_type = function(x) x,
    .options = list(
      substrait$FunctionOption$create(
        name = "overflow",
        preference = "SILENT"
      )
    )
  )
}

duckdb_funs[["exp"]] <- function(x) {
  substrait_call(
    "exp",
    x,
    .output_type = substrait_fp64(),
    .options = list(
      substrait$FunctionOption$create(
        name = "overflow",
        preference = "SILENT"
      )
    )
  )
}

duckdb_funs[["sign"]] <- function(x) {
  substrait_call(
    "sign",
    x,
    .output_type = substrait_fp64(),
    .options = list(
      substrait$FunctionOption$create(
        name = "overflow",
        preference = "SILENT"
      )
    )
  )
}

duckdb_funs[["sum"]] <- function(x, na.rm = FALSE) {
  check_na_rm_duckdb(na.rm)
  substrait_call_agg("sum", x, .output_type = identity)
}

duckdb_funs[["mean"]] <- function(x, na.rm = FALSE) {
  check_na_rm_duckdb(na.rm)
  substrait_call_agg("avg", x, .output_type = substrait_i64())
}

duckdb_funs[["min"]] <- function(x, na.rm = FALSE) {
  check_na_rm_duckdb(na.rm)
  substrait_call_agg("min", x, .output_type = substrait_i64())
}

duckdb_funs[["max"]] <- function(x, na.rm = FALSE) {
  check_na_rm_duckdb(na.rm)
  substrait_call_agg("max", x, .output_type = substrait_i64())
}

duckdb_funs[["n"]] <- function() {
  substrait_call_agg("count", .output_type = substrait_i64())
}

duckdb_funs[["grepl"]] <- function(pattern, x) {
  substrait_call("contains", x, pattern)
}

duckdb_funs[["n_distinct"]] <- function(x, na.rm = FALSE) {
  check_na_rm_duckdb(na.rm)
  warn_n_distinct()

  substrait_call_agg("approx_count_distinct", x, .output_type = substrait_i64())
}

duckdb_funs[["year"]] <- function(x) {
  substrait_call(
    "year",
    x,
    .output_type = substrait_i64()
  )
}

duckdb_funs[["round"]] <- function(x, digits = 0) {
  substrait_call(
    "round",
    x,
    as.integer(digits),
    .output_type = substrait_fp64()
  )
}

duckdb_funs[["ceiling"]] <- function(x) {
  substrait_call(
    "ceil",
    x,
    .options = list(
      substrait$FunctionOption$create(name = "rounding", preference = "TIE_TO_EVEN")
    ),
    .output_type = substrait_fp64()
  )
}

duckdb_funs[["floor"]] <- function(x) {
  substrait_call(
    "floor",
    x,
    .options = list(
      substrait$FunctionOption$create(name = "rounding", preference = "TIE_TO_EVEN")
    ),
    .output_type = substrait_fp64()
  )
}

duckdb_funs[["substr"]] <- function(x, start, stop) {
  substrait_call(
    "substring",
    x,
    as.integer(start),
    as.integer(stop - start + 1)
  )
}

check_na_rm_duckdb <- function(na.rm) {
  if (!na.rm) {
    warning("Missing value removal from aggregate functions not supported in DuckDB, switching to na.rm = TRUE")
  }
}

warn_n_distinct <- function() {
  rlang::warn(
    "n_distinct() currently returns an approximate count",
    .frequency = "once",
    .frequency_id = "substrait.n_distinct.approximate",
    class = "substrait.n_distinct.approximate"
  )
}
