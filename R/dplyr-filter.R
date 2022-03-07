#' Filter column
#'
#' @param .data substrait_Rel object
#' @inheritParams dplyr::filter
#' @importFrom dplyr select
#' @export
filter.substrait_op <- function(.data, ..., .preserve = FALSE) {
  columns <- attr(.data, "cols")

  conditions <- quos(...)

  if (is.empty(conditions)) {
    return(.data)
  }

  structure(
    .data,
    cols = cols,
    filters = conditions,
    class = c("substrait_op", "substrait_filter")
  )
}

#' @rdname build_substrait
#' @export
build_substrait.substrait_filter <- function(x) {
  col_list <- unname(attr(x, "cols"))

  locs <- match(
    unname(vapply(col_list, as.character, character(1))),
    names(x)
  )

  # substrait$FilterRel$create(
  #   condition = list(
  #     scalar_function = list(
  #       function_reference = 1,
  #       args = list(
  #         selection = list(
  #           #direct_reference = list(
  #           #)
  #         )
  #       )
  #     )
  #   )
  # )


  lapply(locs, function(pos) {
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

