#' Expect that a selected field matches an integer value
expect_selected_field <- function(object, expected){
  expect_equal(
    object[["selection"]][["direct_reference"]][["struct_field"]][["field"]],
    expected
  )
}
