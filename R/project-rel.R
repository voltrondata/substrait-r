
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
#'    data.frame(a = 1, b = "one"),
#'    c = a + 1
#' )
#'
substrait_project <- function(.compiler, ...) {
  .compiler <- substrait_compiler(.compiler)$clone()

  context <- list(
    schema = .compiler$schema,
    list_of_expressions = .compiler$mask
  )

  expressions <- lapply(
    rlang::enquos(..., .named = TRUE),
    as_substrait,
    .ptype = "substrait.Expression",
    compiler = .compiler
  )

  types <- lapply(
    expressions,
    as_substrait,
    .ptype = "substrait.Type",
    compiler = .compiler
  )

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
