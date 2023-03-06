
substrait_join <- function(compiler_left, compiler_right, by = NULL,
                           type = "JOIN_TYPE_INNER",
                           name_repair = join_name_repair_suffix_common(),
                           emit = join_emit_all) {
  # Somehow we have to merge these two compilers. If one of them is not yet
  # a compiler (e.g., a data.frame), this is significantly easier (i.e.,
  # we just add a new named table).
  if (inherits(compiler_left, "SubstraitCompiler") &&
    !inherits(compiler_right, "SubstraitCompiler")) {
    compiler <- compiler_left$clone()
    right_read_rel <- compiler$add_named_table(compiler_right)

    left_rel <- compiler$rel
    left_schema <- compiler$schema

    right_rel <- substrait$Rel$create(read = right_read_rel)
    right_schema <- right_read_rel$base_schema
  } else if (!inherits(compiler_left, "SubstraitCompiler") &&
    inherits(compiler_right, "SubstraitCompiler")) {
    compiler <- compiler_right$clone()
    left_read_rel <- compiler$add_named_table(compiler_left)

    left_rel <- substrait$Rel$create(read = left_read_rel)
    left_schema <- left_read_rel$base_schema

    right_rel <- compiler$rel
    right_schema <- compiler$schema
  } else if (!inherits(compiler_left, "SubstraitCompiler") &&
    !inherits(compiler_right, "SubstraitCompiler")) {
    return(
      substrait_join(
        substrait_compiler(compiler_left),
        compiler_right,
        by = by,
        type = type,
        name_repair = name_repair,
        emit = emit
      )
    )
  } else {
    # This is probably hard...we need to walk the extensions and give them
    # new IDs, then walk the relations and all expressions they contain
    # to replace the functions with new IDs.
    rlang::abort("Merging substrait compilers is not yet implemented")
  }

  # Update the compiler schema and mask before evaluating the join expression
  # We haven't applied name_repair or emit yet...these names may contain
  # duplicates (but these names will not be used to resolve field references
  # by as_join_expression()). For the purposes of generating the join
  # expression; however, the compiler needs the full concatenated schema.
  compiler$schema <- substrait$NamedStruct$create(
    names = c(left_schema$names, right_schema$names),
    struct = substrait$Type$Struct$create(
      types = c(left_schema$struct$types, right_schema$struct$types)
    )
  )

  compiler$.data <- lapply(
    seq_along(compiler$schema$struct$types) - 1L,
    simple_integer_field_reference
  )
  names(compiler$.data) <- compiler$schema$names

  # The compiler is needed here because the == and & functions
  # need to be translated and possibly added to the extension set
  expression <- with_compiler(compiler, {
    as_join_expression(by, left_schema$names, right_schema$names)
  })

  # Generate the output mapping (e.g., remove join keys from the
  # righthand side)
  output_mapping <- emit(by, left_schema$names, right_schema$names)

  # Calculate column names (e.g., add suffixes to disambiguate left and
  # right names that both appear in the output)
  names_out <- name_repair(output_mapping, left_schema$names, right_schema$names)

  # Create the relation
  rel <- substrait$Rel$create(
    join = substrait$JoinRel$create(
      common = substrait$RelCommon$create(
        emit = substrait$RelCommon$Emit$create(
          output_mapping = output_mapping
        )
      ),
      left = left_rel,
      right = right_rel,
      expression = expression,
      type = type
    )
  )

  # Update the compiler
  compiler$rel <- rel

  # Reset the schema again to reflect output_mapping and name_repair
  compiler$schema <- substrait$NamedStruct$create(
    names = names_out,
    struct = substrait$Type$Struct$create(
      types = compiler$schema$struct$types[output_mapping + 1L]
    )
  )

  compiler$.data <- lapply(
    seq_along(compiler$schema$struct$types) - 1L,
    simple_integer_field_reference
  )
  names(compiler$.data) <- compiler$schema$names

  compiler$validate()
}

as_join_expression <- function(by, names_left, names_right) {
  by <- sanitize_join_by(by, names_left, names_right)
  by_left <- by$left
  by_right <- by$right

  by_left <- lapply(by_left - 1L, simple_integer_field_reference)
  by_right <- lapply(by_right - 1L + length(names_left), simple_integer_field_reference)

  expr_eq <- Map(function(x, y) substrait_eval(!!x == !!y), by_left, by_right)
  expr_eq <- lapply(expr_eq, as_substrait, "substrait.Expression")
  if (length(expr_eq) == 0) {
    NULL
  } else if (length(expr_eq) == 1) {
    expr_eq[[1]]
  } else {
    expr_all <- Reduce(combine_expressions_and, expr_eq)
    as_substrait(expr_all, "substrait.Expression")
  }
}

# This performs no name repair at all, which may result in non-unique names
# in the output.
join_name_repair_none <- function(output_mapping, names_left, names_right) {
  c(names_left, names_right)[output_mapping + 1L]
}

# This performs dplyr's default behaviour, which is to disambiguate column
# names that appear in both the left and the right by applying a suffix.
join_name_repair_suffix_common <- function(suffix = c(".x", ".y")) {
  stopifnot(is.character(suffix), length(suffix) == 2, all(!is.na(suffix)))

  function(output_mapping, names_left, names_right) {
    names_out <- join_name_repair_none(output_mapping, names_left, names_right)
    names_from_left <- output_mapping < length(names_left)
    names_from_right <- output_mapping >= length(names_left)
    names_needs_suffix <- names_out %in% unique(names_out[duplicated(names_out)])

    suffix_left <- names_from_left & names_needs_suffix
    suffix_right <- names_from_right & names_needs_suffix

    if (any(suffix_left)) {
      names_out[suffix_left] <- paste0(names_out[suffix_left], suffix[1])
    }

    if (any(suffix_right)) {
      names_out[suffix_right] <- paste0(names_out[suffix_right], suffix[2])
    }

    names_out
  }
}


# Usually joins don't emit everything (e.g., join keys are not included
# twice). If everything is emitted, the left and right tables are
# just concatenated. This would usually be confusing because often
# names overlap and it would result in non-unique names, so the default
# is to not include the join key columns from the righthand side of the join.
join_emit_all <- function(by, names_left, names_right) {
  seq_len(length(names_left) + length(names_right)) - 1L
}

join_emit_default <- function(by, names_left, names_right) {
  by <- sanitize_join_by(by, names_left, names_right)

  seq_left <- seq_along(names_left)
  seq_right <- seq_along(names_right)
  seq_right <- setdiff(seq_right, by$right)

  c(seq_left - 1L, seq_right + length(seq_left) - 1L)
}

# Takes a `by` expression like `c("col1", "col2")` or
# `c("col1_left" = "col1_right")` and transforms it into (1-based) indices
# of the left table and right table. If `is.null(by)`, the intersection
# of the common names is used (i.e., interpret `by` just like dplyr does).
sanitize_join_by <- function(by, names_left, names_right) {
  if (is.null(by)) {
    by_left <- intersect(names_left, names_right)
    by_right <- by_left
  } else if (rlang::is_named(by)) {
    by_left <- names(by)
    by_right <- unname(by)
  } else {
    by_left <- by
    by_right <- by
  }

  if (is.character(by_right)) {
    by_left <- match(by_left, names_left)
    by_right <- match(by_right, names_right)
  } else {
    rlang::abort(
      "Only join expressions in the form c('name_left' = 'name_right') are supported"
    )
  }

  list(left = by_left, right = by_right)
}

# For substrait_join() to return without error, a compiler must have &
# and == implemented. This compiler satisfies that minimum requirement.
join_dummy_compiler <- function(df) {
  compiler <- substrait_compiler(df)
  compiler$.fns[["&"]] <- function(lhs, rhs) substrait_call("AND", lhs, rhs)
  compiler$.fns[["=="]] <- function(lhs, rhs) substrait_call("EQUALS", lhs, rhs)
  compiler
}
