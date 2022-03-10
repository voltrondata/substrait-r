#' Take selected columns and create the appropriate substrait message
#'
#' @param projections vector of selected columns
#'
#' @export
build_projections <- function(projections, df){

  # get numeric matches of column positions
  locs <- match(
    unname(vapply(projections, as.character, character(1))),
    names(df)
  )

  expressions <- lapply(locs, function(pos) {
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

  expressions

}
