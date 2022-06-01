
#' dplyr verb implementations
#'
#' @param .data,x A [substrait_compiler()]
#' @param ...
#'   - `select()`: see [dplyr::select()]
#'   - `rename()`: see [dplyr::rename()]
#'   - `filter()`: see [dplyr::filter()]
#'   - `mutate()`: see [dplyr::mutate()]
#'   - `arrange()`: see [dplyr::arrange()]
#' @param .drop Not supported, see [dplyr::group_by()]
#' @param .add Use `TRUE` to add the groupings to the current groupings and
#'   `FALSE` to reset the grouping.
#' @param .groups One of "drop_last", "drop", or "keep"
#'   (see [dplyr::summarise()]).
#'
#' @return A modified [substrait_compiler()]
#' @importFrom dplyr select
#' @export
#'
#' @examplesIf requireNamespace("dplyr", quietly = TRUE)
#' compiler <- substrait_compiler(mtcars)
#' dplyr::select(compiler, mpg2 = mpg)
#' dplyr::rename(compiler, mpg2 = mpg)
#' dplyr::filter(compiler, mpg > 20)
#' dplyr::mutate(compiler, mpg + 10)
#' dplyr::transmute(compiler, mpg + 10)
#' dplyr::arrange(compiler, desc(mpg))
#'
select.SubstraitCompiler <- function(.data, ...) {
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

#' @rdname select.SubstraitCompiler
#' @importFrom dplyr rename
#' @export
rename.SubstraitCompiler <- function(.data, ...) {
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

#' @rdname select.SubstraitCompiler
#' @importFrom dplyr filter
#' @export
filter.SubstraitCompiler <- function(.data, ...) {
  substrait_filter(.data, ...)
}

#' @rdname select.SubstraitCompiler
#' @importFrom dplyr mutate
#' @export
mutate.SubstraitCompiler <- function(.data, ...) {
  mask <- .data$mask
  substrait_project(.data, !!! mask, ...)
}

#' @rdname select.SubstraitCompiler
#' @importFrom dplyr transmute
#' @export
transmute.SubstraitCompiler <- function(.data, ...) {

  check_transmute_args(...)
  substrait_project(.data, ...)
}

#' @rdname select.SubstraitCompiler
#' @importFrom dplyr arrange
#' @export
arrange.SubstraitCompiler <- function(.data, ...) {
  quos <- rlang::enquos(...)

  if (rlang::is_empty(quos)) {
    return(.data)
  }

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

#' @rdname select.SubstraitCompiler
#' @importFrom dplyr group_by
#' @export
group_by.SubstraitCompiler <- function(.data, ..., .add = FALSE, .drop = NULL) {
  if (!is.null(.drop)) {
    stop("`group_by(..., .drop)` is not supported for SubstraitCompiler")
  }

  if (.add) {
    old_groups <- .data$groups
    .data <- substrait_group_by(.data, ...)
    .data$groups <- c(old_groups, .data$groups)
    .data
  } else {
    substrait_group_by(.data, ...)
  }
}

#' @rdname select.SubstraitCompiler
#' @importFrom dplyr ungroup
#' @export
ungroup.SubstraitCompiler <- function(x, ...) {
  substrait_ungroup(x)
}

#' @rdname select.SubstraitCompiler
#' @importFrom dplyr summarise
#' @export
summarise.SubstraitCompiler <- function(.data, ..., .groups = NULL) {
  .groups <- .groups %||% "drop_last"

  n_groups <- length(.data$groups)

  .data <- substrait_aggregate(.data, ...)

  if (identical(.groups, "drop")) {
    .data
  } else if (identical(.groups, "drop_last")) {
    new_n_groups <- max(0, n_groups - 1)
    substrait_group_by(
      .data,
      !!! rlang::syms(.data$schema$names[seq_len(new_n_groups)])
    )
  } else if (identical(.groups, "keep")) {
    substrait_group_by(
      .data,
      !!! rlang::syms(.data$schema$names[seq_len(n_groups)])
    )
  } else {
    stop("Unknown value for `.groups`")
  }
}

#' @rdname select.SubstraitCompiler
#' @importFrom dplyr collect
#' @export
collect.SubstraitCompiler <- function(x, ...) {
  out <- dplyr::as_tibble(x$evaluate(...))
  group_by(out, !!! rlang::syms(names(x$groups)))
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

simulate_data_frame <- function(compiler) {
  from_substrait(compiler$schema, data.frame())
}

check_transmute_args <- function(..., .keep, .before, .after, error_call = rlang::caller_env()){
    if (!missing(.keep)) {
        abort("The `.keep` argument is not supported.", call = error_call)
    }
    if (!missing(.before)) {
        abort("The `.before` argument is not supported.", call = error_call)
    }
    if (!missing(.after)) {
        abort("The `.after` argument is not supported.", call = error_call)
    }
}
