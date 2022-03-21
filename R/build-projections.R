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
    selection = substrait_proto_auto(
      direct_reference = substrait_proto_auto(
        struct_field = substrait_proto_auto(
          # 0-indexed!
          field = pos0,
          child = substrait_proto_auto(
            struct_field = substrait_proto_auto(
              field = substrait_proto_auto()
            )
          )
        )
      )
    )
  )
}
