
substrait_colnames <- function(x) {
  names(substrait_coltypes(x))
}

substrait_coltypes <- function(x) {
  switch(
    class(x)[1],
    "substrait_ReadRel" = {
      types <- x$base_schema$struct_$types
      names(types) <- x$base_schema$names
      types
    },
    "substrait_FilterRel" = ,
    "substrait_SortRel" = substrait_coltypes(x$input),
    "substrait_Rel" = substrait_coltypes(x[[names(x)[1]]]),
    "substrait_PlanRel" = substrait_coltypes(x$rel),
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
