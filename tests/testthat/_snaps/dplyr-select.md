# select() doesn't work on variables that have been excluded in a previous select()

    Code
      base_table(schema) %>% dplyr::select(hp, mpg) %>% dplyr::select(carb)
    Error <vctrs_error_subscript_oob>
      Can't subset columns that don't exist.
      x Column `carb` doesn't exist.

