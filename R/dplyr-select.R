#' Select column
#'
#' @param .data substrait_Rel object
#' @inheritParams dplyr::select
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
  cols <- tidyselect::eval_select(rlang::expr(c(...)), empty_df)

  structure(
    .data,
    cols = rlang::syms(rlang::set_names(columns[cols], names(cols))),
    class = c("substrait_op", "substrait_select")
  )
}


#' Build a substrait expression from a dplyr select object
#'
#' @param x Object of class `substrait_select`
#' @return List of selection expressions
#'
#' @export
build_substrait <- function(x) {
  UseMethod("build_substrait", x)
}

#' @rdname build_substrait
#' @export
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

#' Create a substrait plan from
#'
#' @param x A substrait_op object
#' @inheritParams as_substrait
#' @return A substrait plan
#'
#' @export
as_substrait.substrait_op <- function(x, .ptype = NULL, ...) {
  # create the root relations
  rel <- substrait$PlanRel$create(
    root = substrait$RelRoot$create(
      input = base_table(x),
      names = names(x)
    )
  )
  substrait$Plan$create(relations = list(rel))
}

base_table <- function(df) {
  structure(
    df,
    cols = rlang::syms(names(df)),
    class = c("substrait_op", "substrait_base_table")
  )
}
