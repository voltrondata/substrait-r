
#' Append a Substrait Project Relation
#'
#' @param .compiler A [substrait_compiler()] or object that can be coerced to one
#' @param ... Expressions
#' @param .drop_columns A character vector of columns to explicitly drop before
#'   adding expressions specified in `...`.
#'
#' @return A modified `.compiler`
#' @export
#'
#' @examples
#' substrait_select(
#'   duckdb_substrait_compiler(data.frame(a = 1, b = "one")),
#'   c = a + 1
#' )
#'
substrait_project <- function(.compiler, ..., .drop_columns = character()) {
  .compiler <- substrait_compiler(.compiler)$clone()
  local_compiler(.compiler)

  # Evaluate expressions sequentially, updating the compiler as we go so that
  # fields created by earlier arguments are accessible from later arguments
  quos <- rlang::enquos(..., .named = TRUE)
  expressions <- list()
  types <- list()

  # Keep a list of columns that were specified as explicit NULLs
  quos_that_were_null <- character()

  for (i in seq_along(quos)) {
    name <- names(quos)[i]

    if (!rlang::quo_is_null(quos[[i]])) {
      # Do the evaluation and calculate the output type
      value <- as_substrait(
        quos[[i]],
        .ptype = "substrait.Expression"
      )
      type <- as_substrait(value, .ptype = "substrait.Type")

      # Update the compiler mask (used for symbol lookup for subsequent expressions)
      .compiler$.data[[name]] <- value

      # keep track of the new expressions and types
      expressions[[name]] <- value
      types[[name]] <- type

      # ...and make sure we forget a previous explicit NULL for this name
      quos_that_were_null <- setdiff(quos_that_were_null, name)
    } else {
      # Remove from the compiler mask so that we can't use the NULL column in a
      # subsequent argument (as per dplyr behaviour)
      .compiler$.data[[name]] <- NULL

      # Remove from our list of new expressions to append if it had been
      # previously added
      expressions[[name]] <- NULL
      types[[name]] <- NULL

      # ...and make sure we drop this column if it already existed
      quos_that_were_null <- union(quos_that_were_null, name)
    }
  }

  # Apply NULL quosure arguments to .drop_columns
  .drop_columns <- union(quos_that_were_null, .drop_columns)

  # Create the Emit.output_mapping we need to get our final columns
  names_using_only_append_logic <- c(.compiler$schema$names, names(expressions))
  # The rev()s here are to make sure the column order comes out as expected
  # since unique() keeps the first instance of a value it encounters.
  final_columns <- unique(rev(setdiff(rev(names_using_only_append_logic), .drop_columns)))

  # Match from last item (we want the value that we just calculated to replace
  # the value that previously existed)
  output_mapping <- length(names_using_only_append_logic) +
    1L - match(final_columns, rev(names_using_only_append_logic))

  # Don't include the Emit object if it isn't needed (in case the engine
  # hasn't implemented it)
  if (identical(output_mapping, seq_along(names_using_only_append_logic))) {
    common <- substrait$RelCommon$create()
  } else {
    common <- substrait$RelCommon$create(
      emit = substrait$RelCommon$Emit$create(
        output_mapping = output_mapping - 1L
      )
    )
  }

  # Create the relation with the new expressions and types
  rel <- substrait$Rel$create(
    project = substrait$ProjectRel$create(
      common = common,
      input = .compiler$rel,
      expressions = expressions
    )
  )

  # update the compiler
  .compiler$rel <- rel
  .compiler$schema$names <- final_columns
  .compiler$schema$struct$types <- c(.compiler$schema$struct$types, types)[output_mapping]

  # reset the mask
  .compiler$.data <- lapply(
    seq_along(.compiler$schema$struct$types) - 1L,
    simple_integer_field_reference
  )
  names(.compiler$.data) <- .compiler$schema$names

  .compiler$validate()
}

#' @rdname substrait_project
#' @export
substrait_select <- function(.compiler, ...) {
  .compiler <- substrait_compiler(.compiler)
  quos <- rlang::enquos(..., .named = TRUE)

  final_col_names <- names(quos)
  drop_cols <- setdiff(.compiler$schema$names, final_col_names)

  substrait_project(.compiler, !!! quos, .drop_columns = drop_cols)
}

# Simplify the verbose definition of a field reference
simple_integer_field_reference <- function(pos0) {
  substrait$Expression$create(
    selection = substrait$Expression$FieldReference$create(
      direct_reference = substrait$Expression$ReferenceSegment$create(
        struct_field = substrait$Expression$ReferenceSegment$StructField$create(
          # 0-indexed!
          field = pos0
        )
      ),
      # Required field
      root_reference = substrait$Expression$FieldReference$RootReference$create()
    )
  )
}
