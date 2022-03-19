# Take selected columns and create the appropriate substrait message

build_sort <- function(df, sort_cols, sort_desc) {

  # get numeric matches of column positions
  locs <- match(
    unname(vapply(sort_cols, as.character, character(1))),
    names(df)
  )


  # -1 as it's 0-indexed
  to_sort <- Map(list, field = locs - 1, desc = sort_desc)

  sort_expressions <- lapply(
    to_sort,
    sort_field
  )

  sort_expressions
}

sort_field <- function(ref){

  substrait$SortField$create(
    expr = simple_integer_field_reference(ref$field),
    direction = dplyr_desc_to_substrait(ref$desc)
  )
}

# Convert from dplyr sort order to substrait sort order
dplyr_desc_to_substrait <- function(dplyr_desc) {
  if (dplyr_desc) {
    # SORT_DIRECTION_DESC_NULLS_LAST = 4
    4
  } else {
    # SORT_DIRECTION_ASC_NULLS_LAST = 2
    2
  }
}
