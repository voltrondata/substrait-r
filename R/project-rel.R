
#' Append a Substrait Project Relation
#'
#' @param .compiler A [substrait_compiler()] or object that can be coerced to one
#' @param ... Expressions
#'
#' @return A modified `.compiler`
#' @export
#'
#' @examples
#' substrait_project(
#'   data.frame(a = 1, b = "one"),
#'   c = a + 1
#' )
#'
substrait_project <- function(.compiler, ...) {
  .compiler <- substrait_compiler(.compiler)$clone()

  # evaluate expressions sequentially, updating the compiler as we go so that
  # fields created by earlier arguments are accessible from later arguments
  quos <- rlang::enquos(..., .named = TRUE)
  expressions <- list()
  types <- list()

  for (i in seq_along(quos)) {
    name <- names(quos)[i]
    if (!rlang::quo_is_null(quos[[i]])) {

      # do the evaluation and calculate the output type
      value <- as_substrait(
        quos[[i]],
        .ptype = "substrait.Expression",
        compiler = .compiler
      )
      type <- as_substrait(value, .ptype = "substrait.Type", compiler = .compiler)

      # update the compiler
      .compiler$mask[[name]] <- value
      .compiler$schema$names <- union(.compiler$schema$names, name)
      .compiler$schema$struct_$types[[match(name, .compiler$schema$names)]] <-
        type

      # keep track of the new expressions and types
      expressions[[name]] <- value
      types[[name]] <- type
    } else {
      expressions[[name]] <- NULL
      types[[name]] <- NULL
      # remove from compiler so that we can't use the NULL column later
      # (as per dplyr behaviour)
      .compiler$mask[[name]] <- NULL
      index_match <- match(name, .compiler$schema$names)
      if (!is.na(index_match)) {
        .compiler$schema$struct_$types[[index_match]] <- NULL
      }
      .compiler$schema$names <- .compiler$schema$names[.compiler$schema$names != name]
    }
  }

  # create the relation with the new expressions and types
  rel <- substrait$Rel$create(
    project = substrait$ProjectRel$create(
      input = .compiler$rel,
      expressions = expressions
    )
  )

  # update the compiler
  .compiler$rel <- rel
  .compiler$schema$names <- names(expressions)
  .compiler$schema$struct_$types <- types

  # reset the mask
  .compiler$mask <- lapply(
    seq_along(types) - 1L,
    simple_integer_field_reference
  )
  names(.compiler$mask) <- names(types)

  .compiler$validate()
}

# Take selected columns and create the appropriate substrait message
build_projections <- function(df, projections) {
  # get numeric matches of column positions
  locs <- match(
    unname(vapply(projections, as.character, character(1))),
    names(df)
  )

  # -1 as it's 0-indexed but tidyselect is 1-indexed
  expressions <- lapply(
    locs - 1,
    simple_integer_field_reference
  )

  expressions
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
      )
    )
  )
}
