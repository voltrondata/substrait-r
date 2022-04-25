example_data <- tibble::tibble(
  int = c(1:3, NA_integer_, 5:10),
  dbl = c(1:8, NA, 10) + .1,
  dbl2 = rep(5, 10),
  lgl = sample(c(TRUE, FALSE, NA), 10, replace = TRUE),
  false = logical(10),
  chr = letters[c(1:5, NA, 7:10)],
  #fct = factor(letters[c(1:4, NA, NA, 7:10)]) # factors not currently supported
)

compare_arrow_dplyr_binding <- function(expr, tbl){

  expr <- rlang::enquo(expr)
  expected <- rlang::eval_tidy(expr, rlang::new_data_mask(rlang::env(.input = tbl)))

  out_substrait <- rlang::eval_tidy(expr, rlang::new_data_mask(rlang::env(.input = arrow_substrait_compiler(tbl))))

  expect_identical(out_substrait, expected)

}
