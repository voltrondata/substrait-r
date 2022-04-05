
#' Append a Substrait Project Relation
#'
#' @param .builder A [substrait_compiler()] or object that can be coerced to one
#' @param ... Expressions
#'
#' @return A modified `.builder`
#' @export
#'
#' @examples
#' substrait_project(
#'    data.frame(a = 1, b = "one"),
#'    c = a + 1
#' )
#'
substrait_project <- function(.builder, ...) {
  .builder <- substrait_compiler(.builder)
  .builder$consumer <- .builder$consumer$clone()

  context <- list(
    schema = .builder$schema,
    list_of_expressions = .builder$mask
  )

  expressions <- lapply(
    rlang::enquos(..., .named = TRUE),
    as_substrait,
    .ptype = "substrait.Expression",
    consumer = .builder$consumer,
    context = context
  )

  types <- lapply(
    expressions,
    as_substrait,
    .ptype = "substrait.Type",
    context = context
  )

  rel <- substrait$Rel$create(
    project = substrait$ProjectRel$create(
      input = .builder$rel,
      expressions = expressions
    )
  )

  # update the builder
  .builder$rel <- rel
  .builder$schema$names <- names(expressions)
  .builder$schema$struct_$types <- types
  .builder$mask <- expressions

  .builder$consumer$validate_builder(.builder)
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
          field = pos0,
          # dd: Do we need this? It seems like this would be indexing in to a
          # nested struct (like df$col_that_is_a_df[[0]])?
          child = substrait$Expression$ReferenceSegment$create(
            struct_field = substrait$Expression$ReferenceSegment$StructField$create()
          )
        )
      )
    )
  )
}
