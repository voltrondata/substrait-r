
test_that("substrait_group_by() / substrait_ungroup() (un) set the grouping", {
  df <- data.frame(a = c(1, 1, 2, 2, 3))
  compiler <- substrait_compiler(df)

  grouped <- substrait_group_by(compiler, a)
  expect_identical(
    grouped$groups,
    list(a = simple_integer_field_reference(0))
  )

  ungrouped <- substrait_ungroup(grouped)
  expect_null(ungrouped$groups)
})

test_that("substrait_aggregate() can evaluate a simple aggregation expression", {
  compiler <- substrait_compiler(data.frame(a = c(1, 1, 2, 2, 3), b = 1:5))
  compiler$.fns$sum <- function(x) substrait_call_agg("sum", x)
  compiler$agg_functions <- c("sum")
  compiler$.fns[["+"]] <- function(lhs, rhs) substrait_call("+", lhs, rhs)

  grouped <- substrait_group_by(compiler, a)
  agg <- substrait_aggregate(grouped, c = sum(b + 1))

  expect_s3_class(agg$rel$aggregate, "substrait_AggregateRel")
  expect_identical(
    agg$rel$aggregate$groupings[[1]]$grouping_expressions[[1]],
    simple_integer_field_reference(0)
  )
  expect_s3_class(
    agg$rel$aggregate$measures[[1]]$measure,
    "substrait_AggregateFunction"
  )
  expect_identical(
    agg$function_extension(
      agg$rel$aggregate$measures[[1]]$measure$function_reference
    )$name,
    "sum"
  )
  expect_s3_class(
    agg$rel$aggregate$measures[[1]]$measure$arguments[[1]]$value$scalar_function,
    "substrait_Expression_ScalarFunction"
  )
  expect_identical(agg$schema$names, c("a", "c"))
  expect_identical(
    agg$.data,
    list(
      a = simple_integer_field_reference(0),
      c = simple_integer_field_reference(1)
    )
  )
})

test_that("simple aggregations can be evaluated by DuckDB", {
  skip_if_not(has_duckdb_with_substrait())
  skip_if_not_installed("dplyr")

  df <- data.frame(
    a = c(1, 1, 2, 2, 3),
    b = c(1, 1, 1, 2, 2),
    c = 1:5
  )

  # check zero, one, and two grouping levels
  expect_warning(
    expect_identical(
      df %>%
        duckdb_substrait_compiler() %>%
        substrait_aggregate(c = sum(c + 1)) %>%
        dplyr::collect(),
      tibble::tibble(
        c = as.double(sum(2:6))
      )
    ),
    "Missing value removal from aggregate functions not supported in DuckDB"
  )

  expect_warning(
    expect_identical(
      df %>%
        duckdb_substrait_compiler() %>%
        substrait_group_by(a) %>%
        substrait_aggregate(c = sum(c + 1)) %>%
        dplyr::collect(),
      tibble::tibble(
        a = c(1, 2, 3),
        c = c(5, 9, 6)
      )
    ),
    "Missing value removal from aggregate functions not supported in DuckDB"
  )

  expect_warning(
    expect_identical(
      df %>%
        duckdb_substrait_compiler() %>%
        substrait_group_by(a, b) %>%
        substrait_aggregate(c = sum(c + 1)) %>%
        dplyr::collect(),
      tibble::tibble(
        a = c(1, 2, 2, 3),
        b = c(1, 1, 2, 2),
        c = c(5, 4, 5, 6)
      )
    ),
    "Missing value removal from aggregate functions not supported in DuckDB"
  )


  # check zero measures and >1 measure
  expect_identical(
    df %>%
      duckdb_substrait_compiler() %>%
      substrait_group_by(a, b) %>%
      substrait_aggregate() %>%
      dplyr::collect(),
    tibble::tibble(
      a = c(1, 2, 2, 3),
      b = c(1, 1, 2, 2)
    )
  )

  expect_identical(
    df %>%
      duckdb_substrait_compiler() %>%
      substrait_group_by(a, b) %>%
      substrait_aggregate(
        c = sum(c + 1, na.rm = TRUE),
        d = sum(c, na.rm = TRUE)
      ) %>%
      dplyr::collect(),
    tibble::tibble(
      a = c(1, 2, 2, 3),
      b = c(1, 1, 2, 2),
      c = c(5, 4, 5, 6),
      d = c(3, 3, 4, 5)
    )
  )
})
