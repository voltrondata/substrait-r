# Take selected or mutated columns and create the appropriate substrait message
build_projections <- function(df, projections) {
  # Projections is currently a vector of column names, but it instead needs to be
  # a named list of expressions

  # Actually I think what we need to do here is take each projection and test
  # for whether it contains a field reference or an expression. If a field
  # reference, then use the existing logic here; if an expression, then
  # convert it to a substrait expression

  # First - selections

  # get numeric matches of column positions
  locs <- match(
    unname(vapply(projections, as.character, character(1))),
    names(df)
  )

  # -1 as it's 0-indexed but tidyselect is 1-indexed
  field_references <- lapply(
    locs - 1,
    simple_integer_field_reference
  )

  expressions = NULL

  # Second - mutations

  c(field_references, expressions)
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
