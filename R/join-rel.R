
substrait_join <- function(compiler_left, compiler_right, by = NULL,
                           type = "JOIN_TYPE_INNER",
                           name_repair = join_name_repair_none,
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
  compiler$schema <- substrait$NamedStruct$create(
    names = name_repair(by, left_schema$names, right_schema$names),
    struct = substrait$Type$Struct$create(
      types = c(left_schema$struct$types, right_schema$struct$types)
    )
  )

  compiler$.data <- lapply(
    seq_along(compiler$schema$struct$types) - 1L,
    simple_integer_field_reference
  )
  names(compiler$.data) <- compiler$schema$names

  # The `by` expression resolves field references differently and injects them
  # by value; however, the compiler is needed because the == and & functions
  # need to be translated and added to the extension set
  expression <- with_compiler(compiler, {
    as_join_expression(by, left_schema$names, right_schema$names)
  })

  # Usually joins don't emit everything (e.g., join keys are not included
  # twice). The default behaviour is to concatenate the left and right
  # tables.
  output_mapping <- emit(by, left_schema$names, right_schema$names)

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
  compiler$validate()
}

as_join_expression <- function(by, names_left, names_right) {
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

join_name_repair_none <- function(by, names_left, names_right) {
  c(names_left, names_right)
}

join_emit_all <- function(by, names_left, names_right) {
  seq_len(length(names_left) + length(names_right))
}
