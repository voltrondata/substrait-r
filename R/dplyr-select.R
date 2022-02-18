#' @param .data
select.substrait_Rel <- function(.data, ...){

  columns <- .data$read$base_schema$names

  empty_df <- data.frame(
    matrix(
      ncol = length(columns),
      nrow = 0,
      dimnames = list(NULL, columns)
    )
  )

  locations <- tidyselect::eval_select(expr(c(...)), empty_df)

  name = "unbound_table2"

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

  # TODO: check if this should be pos-1 as it's 0 indexed??
  lapply(cols, function(pos){
    substrait$Expression$create(
      selection = list(
        direct_reference = list(
          struct_field = list(
            field = pos,
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

# rename this function later - currently just copying Python
unbound_table <- function(df, name){
  schema <- as_substrait(df, .ptype = "substrait.NamedStruct")
  substrait$Rel$create(
    read = substrait$ReadRel$create(
      base_schema = schema,
      named_table = substrait$ReadRel$NamedTable$create(names = name)
    )
  )
}
