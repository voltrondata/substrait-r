
substrait_rel_schema <- function(x) {
  switch(
    class(x)[1],
    "substrait_ReadRel" = x$base_schema,
    "substrait_FilterRel" = ,
    "substrait_SortRel" = substrait_rel_schema(x$input),
    "substrait_Rel" = substrait_rel_schema(x[[names(x)[1]]]),
    "substrait_PlanRel" = substrait_rel_schema(x$rel),
    stop(
      sprintf(
        "Can't extract schema from relation of class %s",
        paste(class(x), collapse = " / ")
      )
    )
  )
}

substrait_rel_mask <- function(x) {
  switch(
    class(x)[1],
    "substrait_ReadRel" = {
      schema <- substrait_rel_schema(x)
      mask <- lapply(
        seq_along(schema$names) - 1L,
        simple_integer_field_reference
      )
      names(mask) <- schema$names
      mask
    },
    "substrait_FilterRel" = ,
    "substrait_SortRel" = substrait_rel_mask(x$input),
    "substrait_Rel" = substrait_rel_mask(x[[names(x)[1]]]),
    "substrait_PlanRel" = substrait_rel_mask(x$rel),
    NULL
  )
}

rel_tree_modify <- function(x, classes = character(), fun = identity) {
  if (inherits(x, classes)) {
    fun(x)
  } else if (inherits(x, "substrait_proto_message")) {
    x_items <- lapply(x, rel_tree_modify, classes, fun)
    substrait_create(make_qualified_name(x), !!! x_items)
  } else if (rlang::is_bare_list(x)) {
    lapply(x, rel_tree_modify, classes, fun)
  } else {
    x
  }
}
