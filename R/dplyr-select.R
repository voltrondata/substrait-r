#' Select column
#'
#' @param .data substrait_Rel object
#' @importFrom dplyr select
#' @export
select.substrait_op <- function(.data, ...) {
  columns <- attr(.data, "cols")

  empty_df <- data.frame(
    matrix(
      ncol = length(columns),
      nrow = 0,
      dimnames = list(NULL, columns)
    )
  )

  # Named vector of column names/indices
  cols <- tidyselect::eval_select(expr(c(...)), empty_df)

  structure(
    .data,
    cols = syms(set_names(columns[cols], names(cols))),
    class = c("substrait_op", "substrait_select")
  )
}

#' @export
build_substrait <- function(x) {
  UseMethod("build_substrait", x)
}

#' @export
#' @return List of selection expressions
build_substrait.substrait_select <- function(x) {
  col_list <- unname(attr(x, "cols"))

  locs <- match(
    unname(vapply(col_list, as.character, character(1))),
    names(x)
  )

  lapply(locs, function(pos) {
    substrait$Expression$create(
      selection = list(
        direct_reference = list(
          struct_field = list(
            # -1 as it's 0-indexed but tidyselect is 1-indexed
            field = pos - 1,
            child = list(
              struct_field = list(
                field = list()
              )
            )
          )
        )
      )
    )
  })
}

#' @param x A substrait_op object
#' @export
#' @return A substrait plan
as_substrait.substrait_op <- function(x) {
  schema <- get_schema(x)

  # create the root relations
  rel <- substrait$PlanRel$create(
    root = substrait$RelRoot$create(
      input = base_table(x),
      names = names(schema)
    )
  )
  substrait$Plan$create(relations = list(rel))
}

base_table <- function(df) {
  structure(
    df,
    cols = syms(names(df)),
    class = c("substrait_op", "substrait_base_table")
  )
}
