#' Filter column
#'
#' @param .data substrait_dplyr_query object or data.frame
#' @inheritParams dplyr::filter
#' @importFrom dplyr filter
#' @export
filter.substrait_dplyr_query <- function(.data, ..., .preserve = FALSE) {

  conditions <- rlang::quos(...)

  if (rlang::is_empty(conditions)) {
    return(.data)
  }

  existing_conditions <- attr(.data, "filtered_rows")

  updated_conditions <- append(
    # as.list in case it's NULL
    as.list(existing_conditions),
    conditions
  )

  substrait_dplyr_query(.data, filtered_rows = updated_conditions)
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

