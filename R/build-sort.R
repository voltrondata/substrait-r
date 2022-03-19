# Take selected columns and create the appropriate substrait message
build_sort <- function(df, sort_cols) {
  # get numeric matches of column positions
  locs <- match(
    unname(vapply(sort_cols, as.character, character(1))),
    names(df)
  )

  # -1 as it's 0-indexed
  sort_expressions <- lapply(
    locs - 1,
    sort_field
  )

  sort_expressions
}

sort_field <- function(ref){
  substrait$SortField$create(expr = simple_integer_field_reference(ref))
}
