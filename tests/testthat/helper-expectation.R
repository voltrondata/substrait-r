
compare_arrow_dplyr_binding <- function(expr, tbl){

  expr <- rlang::enquo(expr)
  expected <- rlang::eval_tidy(expr, rlang::new_data_mask(rlang::env(.input = tbl)))

  out_substrait <- rlang::eval_tidy(expr, rlang::new_data_mask(rlang::env(.input = arrow_substrait_compiler(tbl))))

  expect_identical(out_substrait, expected)

}
