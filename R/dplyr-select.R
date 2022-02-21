#' @param .data substrait_Rel object
#' @export
select.substrait_Rel <- function(.data, ...){

  # here we need to be able to work out if .data is a base table or not.
  # if it is, then we run the code below. if it is not (e.g. if it's a project)
  # then we want to be able to combine the project things together
  #
  # do we want to add a class to the substraitRel object? or create a new
  # custom object which itself encapsulates the substraitRel object?

  columns <- .data$read$base_schema$names

  empty_df <- data.frame(
    matrix(
      ncol = length(columns),
      nrow = 0,
      dimnames = list(NULL, columns)
    )
  )

  locations <- tidyselect::eval_select(expr(c(...)), empty_df)

  substrait$Rel$create(
    project = substrait$ProjectRel$create(
      input = .data,
      expressions = get_expressions(locations)
    )
  )

}


#' Construct Expression objects based on column numbers
#'
#' @param col Vector of column numbers
#' @return List of Substrait Expressions
get_expressions <- function(cols){

  lapply(cols, function(pos){
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

base_table <- function(df){
  tbl_expr <- rlang::enexpr(df)
  schema <- as_substrait(df, .ptype = "substrait.NamedStruct")
  substrait$Rel$create(
    read = substrait$ReadRel$create(
      base_schema = schema,
      named_table = substrait$ReadRel$NamedTable$create(names = as.character(tbl_expr))
    )
  )
}
