
#' dplyr verb implementations
#'
#' @param .data,x,y A [substrait_compiler()]
#' @param ...
#'   - `select()`: see [dplyr::select()]
#'   - `rename()`: see [dplyr::rename()]
#'   - `filter()`: see [dplyr::filter()]
#'   - `mutate()`: see [dplyr::mutate()]
#'   - `arrange()`: see [dplyr::arrange()]
#'   - `count()`: see [dplyr::count()]
#'   - `distinct()`: see [dplyr::distinct()]
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
#' @param keep For joins, use `TRUE` to keep all output columns;
#'   see [dplyr::inner_join()].
#' @param by For joins, a join specifier or `NULL` to use common variables
#'   across `x` and `y`; see [dplyr::inner_join()].
#' @param suffix A suffix used to disambiguate columns from `x` and `y` if a
#'   join would result in duplicate column names.
#' @param .keep_all If TRUE, keep all variables in .data; see [dplyr::distinct()]
#' @param wt Frequency weights; see [dplyr::count()]
#' @param sort If TRUE, will show the largest groups at the top; see [dplyr::count()]
#' @param name The name of the new column in the output; see [dplyr::count()]
#'
#' @return A modified [substrait_compiler()]
#' @importFrom dplyr select
#' @export
#'
#' @examplesIf has_duckdb_with_substrait()
#' library(dplyr)
#' compiler <- duckdb_substrait_compiler(mtcars)
#'
#' select(compiler, mpg2 = mpg) %>% collect()
#' rename(compiler, mpg2 = mpg) %>% collect()
#' filter(compiler, mpg > 20) %>% collect()
#' mutate(compiler, mpg + 10) %>% collect()
#' transmute(compiler, mpg + 10) %>% collect()
#' arrange(compiler, desc(mpg)) %>% collect()
#'
select.SubstraitCompiler <- function(.data, ...) {
  sim_data <- simulate_data_frame(.data)

  column_indices <- tidyselect::eval_select(rlang::expr(c(...)), sim_data)

  # restore groups
  if (!rlang::is_empty(.data$groups)) {
    selected_cols <- vapply(rlang::enquos(...), rlang::quo_name, character(1))
    renamed <- selected_cols[names(column_indices) != selected_cols]

    # add in missing columns
    missing_cols <- setdiff(names(.data$groups), selected_cols)
    renamed_groups <- selected_cols[!names(selected_cols) %in% selected_cols]

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

    # ensure any group columns are being renamed
    gbv <- .data$groups
    renamed_groups <- names(gbv) %in% renamed
    names(gbv)[renamed_groups] <- names(renamed)[match(names(gbv[renamed_groups]), renamed)]
    .data <- .data$clone()

    .data$groups <- gbv
  }

  new_mask <- rlang::set_names(
    rlang::syms(.data$schema$names[column_indices]),
    names(column_indices)
  )

  substrait_select(.data, !!!new_mask)
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

  column_names <- names(.data$.data)
  new_column_names <- column_names
  new_column_names[column_indices] <- names(column_indices)

  new_mask <- rlang::set_names(
    rlang::syms(column_names),
    new_column_names
  )

  substrait_select(.data, !!!new_mask)
}

#' @rdname select.SubstraitCompiler
#' @importFrom dplyr rename_with
#' @export
rename_with.SubstraitCompiler <- function(.data, .fn, .cols = dplyr::everything(), ...) {
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
  mask <- .data$.data

  out <- substrait_select(.data, !!!mask, ...)
  if (.keep == "all") {
    return(out)
  }

  # if only keeping a subset of columns, work out which and project again
  cols <- names(mutate(simulate_data_frame(.data), ..., .keep = .keep))
  substrait_select(out, !!!out$.data[cols], !!!rlang::syms(cols))
}

#' @rdname select.SubstraitCompiler
#' @importFrom dplyr transmute
#' @export
transmute.SubstraitCompiler <- function(.data, ...) {
  check_transmute_args(...)
  substrait_select(.data, ...)
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
    out <- dplyr::grouped_df(out, names(x$groups))
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
    rlang::abort("Can't supply both `.before` and `.after`.")
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

  column_names <- names(.data$.data)
  new_column_names <- column_names
  new_column_names[column_indices] <- names(column_indices)

  new_mask <- rlang::set_names(
    rlang::syms(.data$schema$names[pos]),
    new_column_names[pos]
  )

  substrait_select(.data, !!!new_mask)
}

#' @rdname select.SubstraitCompiler
#' @importFrom dplyr semi_join
#' @export
semi_join.SubstraitCompiler <- function(x, y, by = NULL, ...) {
  rlang::check_dots_empty()

  # Semi join support is limited so just do an inner join emitting only
  # columns from the left.
  substrait_join(
    x, y,
    by = by,
    type = "JOIN_TYPE_INNER",
    output_mapping_func = function(by, names_left, names_right) seq_along(names_left) - 1L,
    name_repair_func = join_name_repair_none()
  )
}

#' @rdname select.SubstraitCompiler
#' @importFrom dplyr anti_join
#' @export
anti_join.SubstraitCompiler <- function(x, y, by = NULL, ...) {
  rlang::check_dots_empty()

  joined <- substrait_join(
    x, y,
    by = by,
    type = "JOIN_TYPE_ANTI",
    output_mapping_func = join_emit_all(),
    name_repair_func = join_name_repair_none()
  )

  # A join emit doesn't seem to work with DuckDB yet, so to make this work
  # we add a project instead of applying the output mapping at the join step
  select_args <- rlang::set_names(seq_along(x$schema$names), x$schema$names)
  dplyr::select(joined, !!!select_args)
}

#' @rdname select.SubstraitCompiler
#' @importFrom dplyr inner_join
#' @export
inner_join.SubstraitCompiler <- function(x, y, by = NULL, suffix = c(".x", ".y"),
                                         ..., keep = NULL) {
  dplyr_mutating_join("JOIN_TYPE_INNER", x, y, by = by, suffix = suffix, ..., keep = keep)
}

#' @rdname select.SubstraitCompiler
#' @importFrom dplyr left_join
#' @export
left_join.SubstraitCompiler <- function(x, y, by = NULL, suffix = c(".x", ".y"),
                                        ..., keep = NULL) {
  dplyr_mutating_join("JOIN_TYPE_LEFT", x, y, by = by, suffix = suffix, ..., keep = keep)
}

#' @rdname select.SubstraitCompiler
#' @importFrom dplyr right_join
#' @export
right_join.SubstraitCompiler <- function(x, y, by = NULL, suffix = c(".x", ".y"),
                                         ..., keep = NULL) {
  dplyr_mutating_join("JOIN_TYPE_RIGHT", x, y, by = by, suffix = suffix, ..., keep = keep)
}

#' @rdname select.SubstraitCompiler
#' @importFrom dplyr full_join
#' @export
full_join.SubstraitCompiler <- function(x, y, by = NULL, suffix = c(".x", ".y"),
                                        ..., keep = NULL) {
  dplyr_mutating_join("JOIN_TYPE_OUTER", x, y, by = by, suffix = suffix, ..., keep = keep)
}

dplyr_mutating_join <- function(join_type, x, y, by = NULL, suffix = c(".x", ".y"),
                                ..., keep = NULL) {
  rlang::check_dots_empty()
  keep <- keep %||% FALSE

  joined <- substrait_join(
    x, y,
    by = by,
    type = join_type,
    # A join emit doesn't seem to work with DuckDB yet, so to make this work
    # we add a project if needed instead of applying the output mapping
    # at the join step
    output_mapping_func = join_emit_all(),
    name_repair_func = join_name_repair_suffix_common(suffix)
  )

  if (!keep) {
    names_left <- x$schema$names
    names_right <- as_substrait(y, "substrait.NamedStruct")$names
    output_mapping <- join_emit_default()(by, names_left, names_right)
    output_names <- join_name_repair_suffix_common(suffix)(
      output_mapping,
      names_left,
      names_right
    )

    select_args <- rlang::set_names(output_mapping + 1L, output_names)
    joined <- dplyr::select(joined, !!!select_args)
  }

  joined
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
    rlang::abort("The `.keep` argument is not supported.", call = error_call)
  }
  if (!missing(.before)) {
    rlang::abort("The `.before` argument is not supported.", call = error_call)
  }
  if (!missing(.after)) {
    rlang::abort("The `.after` argument is not supported.", call = error_call)
  }
}

#' @rdname select.SubstraitCompiler
#' @importFrom dplyr distinct
#' @export
distinct.SubstraitCompiler <- function(.data, ..., .keep_all = FALSE) {
  if (.keep_all == TRUE) {
    rlang::abort("`distinct()` with `.keep_all = TRUE` not supported")
  }

  args <- rlang::quos(...)

  original_gv <- dplyr::group_vars(.data)

  if (length(args) > 0) {
    # group_by() calls mutate() if there are any expressions in ...
    .data <- dplyr::group_by(.data, !!!args, .add = TRUE)
  } else {
    # distinct() with no vars specified means distinct across all cols
    .data <- dplyr::group_by(.data, !!!rlang::syms(names(.data$.data)))
  }

  .data <- dplyr::summarize(.data, .groups = "drop")
  # distinct() doesn't modify group by vars, so restore the original ones
  if (length(original_gv) > 0) {
    .data <- dplyr::group_by(.data, !!!rlang::syms(original_gv))
  }

  .data
}

#' @rdname select.SubstraitCompiler
#' @importFrom dplyr count
#' @export
count.SubstraitCompiler <- function(.data, ..., wt = NULL, sort = FALSE, name = NULL) {
  if (!is.null(wt)) {
    rlang::abort("`count()` with `wt != NULL` not supported")
  }

  if (sort) {
    rlang::abort("`count()` with `sort = TRUE` not supported")
  }

  if (!is.null(name)) {
    rlang::abort("`count()` with `name != NULL` not supported")
  }

  grps <- .data$groups

  out <- dplyr::summarise(dplyr::group_by(.data, !!!grps, ...), n = n())

  if (!is.null(grps)) {
    out <- dplyr::group_by(out, !!!rlang::syms(names(grps)))
  } else {
    out <- dplyr::ungroup(out)
  }

  out
}

#' @importFrom dplyr group_vars
#' @export
group_vars.SubstraitCompiler <- function(x) {
  names(x$groups)
}
