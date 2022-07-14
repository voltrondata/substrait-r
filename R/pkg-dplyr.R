
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
#' @param .by_group sort by grouping variable? see[dplyr::arrange()]
#' @param .cols Columns to rename; see[dplyr::rename_with()]
#' @param .fn Function to transform selected `.cols`; see[dplyr::rename_with()]
#' @param .before Destination of columns to move; see [dplyr::relocate()]
#' @param .after Destination of columns to move; see [dplyr::relocate()]
#' @param .keep Which columns are retained in output; see [dplyr::mutate()]
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

  sim_data <- simulate_data_frame(.data)

  column_indices <- tidyselect::eval_select(
    rlang::expr(c(...)),
    sim_data
  )

  # restore groups
  if (!rlang::is_empty(.data$groups)) {
    cols_used <- vapply(rlang::enquos(...), rlang::quo_name, character(1))
    missing_cols <- setdiff(names(.data$groups), cols_used)

    if (!rlang::is_empty(missing_cols)) {
      prepend_cols <- tidyselect::eval_select(
        missing_cols,
        sim_data
      )

      column_indices <- c(prepend_cols, column_indices)

      rlang::inform(
        paste(
          "Adding missing grouping variables:",
          paste0("`", names(prepend_cols), "`", collapse = ", ")
        ),
        fill = TRUE
      )
    }
  }

  new_mask <- rlang::set_names(
    rlang::syms(.data$schema$names[column_indices]),
    names(column_indices)
  )

  substrait_project(.data, !!!new_mask)
}

#' @rdname select.SubstraitCompiler
#' @importFrom dplyr rename
#' @export
rename.SubstraitCompiler <- function(.data, ...) {
  # Named vector of column names/indices
  column_indices <- tidyselect::eval_rename(
    rlang::expr(c(...)),
    simulate_data_frame(.data)
  )

  column_names <- names(.data$mask)
  new_column_names <- column_names
  new_column_names[column_indices] <- names(column_indices)

  new_mask <- rlang::set_names(
    rlang::syms(column_names),
    new_column_names
  )

  substrait_project(.data, !!!new_mask)
}

#' @rdname select.SubstraitCompiler
#' @importFrom dplyr rename_with
#' @export
rename_with.SubstraitCompiler <- function(.data, .fn, .cols = everything(), ...) {
  .fn <- rlang::as_function(.fn)
  old_names <- dplyr::select(.data, {{ .cols }})$schema$names
  dplyr::rename(.data, !!rlang::set_names(old_names, .fn(old_names)))
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
mutate.SubstraitCompiler <- function(.data, ...,
                                     .keep = c("all", "used", "unused", "none")) {
  .keep <- match.arg(.keep)
  mask <- .data$mask

  out <- substrait_project(.data, !!!mask, ...)
  if (.keep == "all") {
    return(out)
  }

  # if only keeping a subset of columns, work out which and project again
  cols <- names(mutate(simulate_data_frame(.data), ..., .keep = .keep))
  substrait_project(out, !!!exprs(cols), ...)
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
arrange.SubstraitCompiler <- function(.data, ..., .by_group = FALSE) {
  if (.by_group) {
    quos <- rlang::quos(!!!rlang::syms(names(.data$groups)), ...)
  } else {
    quos <- rlang::enquos(...)
  }

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

  substrait_sort(.data, !!!with_translated_desc)
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
      !!!rlang::syms(.data$schema$names[seq_len(new_n_groups)])
    )
  } else if (identical(.groups, "keep")) {
    substrait_group_by(
      .data,
      !!!rlang::syms(.data$schema$names[seq_len(n_groups)])
    )
  } else {
    stop("Unknown value for `.groups`")
  }
}

#' @rdname select.SubstraitCompiler
#' @importFrom dplyr summarize
#' @export
summarize.SubstraitCompiler <- summarise.SubstraitCompiler

#' @rdname select.SubstraitCompiler
#' @importFrom dplyr collect
#' @export
collect.SubstraitCompiler <- function(x, ...) {
  out <- dplyr::as_tibble(x$evaluate(...))

  # add back in grouping if needed
  if (!rlang::is_empty(x$groups)) {
    out <- dplyr::grouped_df(out, intersect(names(out), names(x$groups)))
  }

  out
}

#' @rdname select.SubstraitCompiler
#' @importFrom dplyr relocate
#' @export
relocate.SubstraitCompiler <- function(.data, ..., .before = NULL, .after = NULL) {
  to_move <- tidyselect::eval_select(rlang::expr(c(...)), simulate_data_frame(.data))

  .before <- rlang::enquo(.before)
  .after <- rlang::enquo(.after)
  has_before <- !rlang::quo_is_null(.before)
  has_after <- !rlang::quo_is_null(.after)

  if (has_before && has_after) {
    rlang::abort("Must supply only one of `.before` and `.after`.")
  } else if (has_before) {
    where <- min(unname(tidyselect::eval_select(.before, simulate_data_frame(.data))))
    if (!where %in% to_move) {
      to_move <- c(to_move, where)
    }
  } else if (has_after) {
    where <- max(unname(tidyselect::eval_select(.after, simulate_data_frame(.data))))
    if (!where %in% to_move) {
      to_move <- c(where, to_move)
    }
  } else {
    where <- 1L
    if (!where %in% to_move) {
      to_move <- c(to_move, where)
    }
  }

  lhs <- setdiff(rlang::seq2(1, where - 1), to_move)
  rhs <- setdiff(rlang::seq2(where + 1, ncol(simulate_data_frame(.data))), to_move)

  pos <- unique(c(lhs, to_move, rhs))

  column_indices <- tidyselect::eval_select(
    rlang::expr(c(...)),
    simulate_data_frame(.data)
  )

  column_names <- names(.data$mask)
  new_column_names <- column_names
  new_column_names[column_indices] <- names(column_indices)

  new_mask <- rlang::set_names(
    rlang::syms(.data$schema$names[pos]),
    new_column_names[pos]
  )

  substrait_project(.data, !!!new_mask)
}

# translate desc() call to the equivalent
expr_replace_desc <- function(expr) {
  if (rlang::is_call(expr, "desc")) {
    sort_direction <- "SORT_DIRECTION_DESC_NULLS_LAST"

    while (rlang::is_call(expr, "desc")) {
      if (length(expr) > 2) {
        rlang::abort("`desc()` must be called with exactly one argument.", call = parent.frame(3))
      }

      if (rlang::is_call(expr[[2]], "desc")) {
        sort_direction <- swap_sort_direction(sort_direction)
        expr <- expr[[2]]
      } else {
        break
      }
    }

    expr[[1]] <- rlang::sym("substrait_sort_field")
    expr[[3]] <- sort_direction
    expr
  } else {
    expr
  }
}

swap_sort_direction <- function(sort_direction) {
  switch(sort_direction,
    "SORT_DIRECTION_DESC_NULLS_LAST" = "SORT_DIRECTION_ASC_NULLS_LAST",
    "SORT_DIRECTION_ASC_NULLS_LAST" = "SORT_DIRECTION_DESC_NULLS_LAST",
    stop(sprintf("Unsupported sort direction: '%s'"), sort_direction)
  )
}

simulate_data_frame <- function(compiler) {
  from_substrait(compiler$schema, data.frame())
}

check_transmute_args <- function(..., .keep, .before, .after, error_call = rlang::caller_env()) {
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
