
#' dplyr verb implementations
#'
#' @param .data A [substrait_builder()]
#' @param ...
#'   - `select()`: see [dplyr::select()]
#'   - `rename()`: see [dplyr::rename()]
#'   - `filter()`: see [dplyr::filter()]
#'   - `mutate()`: see [dplyr::mutate()]
#'   - `arrange()`: see [dplyr::arrange()]
#'
#' @return A modified [substrait_builder()]
#' @importFrom dplyr select
#' @export
#'
#' @examplesIf requireNamespace("dplyr", quietly = TRUE)
#' builder <- substrait_builder(mtcars)
#' dplyr::select(builder, mpg2 = mpg)
#' dplyr::rename(builder, mpg2 = mpg)
#' dplyr::filter(builder, mpg > 20)
#' dplyr::mutate(builder, mpg + 10)
#' dplyr::transmute(builder, mpg + 10)
#' dplyr::arrange(builder, desc(mpg))
#'
select.substrait_builder <- function(.data, ...) {
  # Named vector of column names/indices
  column_indices <- tidyselect::eval_select(
    rlang::expr(c(...)),
    from_substrait(.data$schema, data.frame())
  )

  new_mask <- rlang::set_names(
    rlang::syms(.data$schema$names[column_indices]),
    names(column_indices)
  )

  substrait_project(.data, !!! new_mask)
}

#' @rdname select.substrait_builder
#' @importFrom dplyr rename
#' @export
rename.substrait_builder <- function(.data, ...) {
  # Named vector of column names/indices
  column_indices <- tidyselect::eval_rename(
    rlang::expr(c(...)),
    from_substrait(.data$schema, data.frame())
  )

  column_names <- names(.data$mask)
  new_column_names <- column_names
  new_column_names[column_indices] <- names(column_indices)

  new_mask <- rlang::set_names(
    rlang::syms(column_names),
    new_column_names
  )

  substrait_project(.data, !!! new_mask)
}

#' @rdname select.substrait_builder
#' @importFrom dplyr filter
#' @export
filter.substrait_builder <- function(.data, ...) {
  substrait_filter(.data, ...)
}

#' @rdname select.substrait_builder
#' @importFrom dplyr mutate
#' @export
mutate.substrait_builder <- function(.data, ...) {
  mask <- .data$mask
  substrait_project(.data, !!! mask, ...)
}

#' @rdname select.substrait_builder
#' @importFrom dplyr transmute
#' @export
transmute.substrait_builder <- function(.data, ...) {
  substrait_project(.data, ...)
}

#' @rdname select.substrait_builder
#' @importFrom dplyr arrange
#' @export
arrange.substrait_builder <- function(.data, ...) {
  quos <- rlang::enquos(...)

  with_translated_desc <- lapply(
    quos,
    function(quo) {
      rlang::quo_set_expr(
        quo,
        expr_replace_desc(rlang::quo_get_expr(quo))
      )
    }
  )

  substrait_sort(.data, !!! with_translated_desc)
}

# translate desc() call to the equivalent
expr_replace_desc <- function(expr) {
  if (rlang::is_call(expr, "desc")) {
    expr[[1]] <- rlang::sym("substrait_sort_field")
    expr[[3]] <- "SORT_DIRECTION_DESC_NULLS_LAST"
    expr
  } else {
    expr
  }
}

simulate_data_frame <- function(builder) {
  from_substrait(builder$schema, data.frame())
}
