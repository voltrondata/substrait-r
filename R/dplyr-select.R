#' Select column
#'
#' @param .data substrait_Rel object
#' @importFrom dplyr select
#' @export
select.substrait_op <- function(.data, ...) {

  columns <- attr(.data, "cols")

  empty_df <- data.frame(
    matrix(
      ncol = length(columns),
      nrow = 0,
      dimnames = list(NULL, columns)
    )
  )

  # Named vector of column names/indices
  cols <- tidyselect::eval_select(expr(c(...)), empty_df)

  structure(
    .data,
    cols = syms(set_names(columns[cols], names(cols))),
    class = c("substrait_op", "substrait_select")
  )

}

#' @export
build_substrait <- function(x){
  UseMethod("build_substrait", x)
}

#' @export
#' @return List of selection expressions
build_substrait.substrait_select <- function(x){

  col_list <- unname(attr(x, "cols"))

  locs <- match(
    unname(vapply(col_list, FUN = as.character, character(1))),
    names(x)
  )

  lapply(locs, function(pos){
    substrait$Expression$create(
      selection = list(
        direct_reference = list(
          struct_field = list(
            # -1 as it's 0-indexed but tidyselect is 1-indexed
            field = pos - 1,
            child = list(
              struct_field = list(
                field = list()
              )
            )
          )
        )
      )
    )
  })
}

#' @param x A substrait_op object
#' @export
#' @return A substrait plan
as_substrait.substrait_op <- function(x) {

  schema <- get_schema(x)

  # create the root relations
  rel <- substrait$PlanRel$create(
    root = substrait$RelRoot$create(
      input = base_table(x),
      names = names(schema)
    )
  )
  substrait$Plan$create(relations = list(rel))


}
 # # I think that this code block should be pulled out into another function and here we just generate the select expressions
 #  # and call this function from within it; this is too early
 #  substrait$Rel$create(
 #    project = substrait$ProjectRel$create(
 #      # Need function here to create the substrait object instead of NULL
 #      input = .data,
 #      expressions = get_expressions(locs)
 #    )
 #  )


base_table <- function(df) {
  structure(
    df,
    cols = syms(names(df)),
    class = c("substrait_op", "substrait_base_table")
  )
}

# base_table <- function(df){
#   tbl_expr <- rlang::enexpr(df)
#   schema <- as_substrait(df, .ptype = "substrait.NamedStruct")
#   # substrait$Rel$create(
#   #   read = substrait$ReadRel$create(
#   #     base_schema = schema,
#   #     named_table = substrait$ReadRel$NamedTable$create(names = as.character(tbl_expr))
#   #   )
#   # )
# }
