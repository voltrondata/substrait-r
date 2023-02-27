skip_if_not(has_duckdb_with_substrait())

test_that("duckdb_substrait_compiler() works", {
  skip_if_not(has_duckdb_with_substrait())

  df <- data.frame(a = 1, b = "two", stringsAsFactors = FALSE)
  compiler <- duckdb_substrait_compiler(df)

  expect_s3_class(compiler, "DuckDBSubstraitCompiler")
  result <- as.data.frame(compiler$evaluate())
  expect_identical(as.data.frame(result), df)
})

test_that("duckdb translation for == and != works", {
  skip_if_not(has_duckdb_with_substrait())
  tbl <- tibble::tibble(col = c(1, 2, NA))

  expect_identical(
    tbl %>%
      duckdb_substrait_compiler() %>%
      dplyr::transmute(eq2 = col == 2, neq2 = col != 2) %>%
      dplyr::collect(),
    tibble::tibble(
      eq2 = c(FALSE, TRUE, NA),
      neq2 = c(TRUE, FALSE, NA)
    )
  )
})

test_that("duckdb translation for & and |", {
  skip_if_not(has_duckdb_with_substrait())
  tbl <- tibble::tibble(col = c(TRUE, FALSE, NA))

  expect_identical(
    tbl %>%
      duckdb_substrait_compiler() %>%
      dplyr::transmute(
        and_true = col & TRUE,
        and_false = col & FALSE,
        or_true = col | TRUE,
        or_false = col | FALSE
      ) %>%
      dplyr::collect(),
    tibble::tibble(
      and_true = c(TRUE, FALSE, NA),
      and_false = c(FALSE, FALSE, FALSE),
      or_true = c(TRUE, TRUE, TRUE),
      or_false = c(TRUE, FALSE, NA)
    )
  )
})

test_that("duckdb translation for ! works", {
  skip_if_not(has_duckdb_with_substrait())

  tbl <- tibble::tibble(col = c(TRUE, FALSE))
  expect_identical(
    tbl %>%
      duckdb_substrait_compiler() %>%
      dplyr::transmute(
        not = !col
      ) %>%
      dplyr::collect(),
    tibble::tibble(
      not = c(FALSE, TRUE)
    )
  )
})

test_that("duckdb translation for ! handles NULL", {
  skip_if_not(has_duckdb_with_substrait())
  skip("duckdb translation for ! doesn't handle NULLs")

  tbl <- tibble::tibble(col = c(TRUE, FALSE, NA))
  expect_identical(
    tbl %>%
      duckdb_substrait_compiler() %>%
      dplyr::transmute(
        not = !col
      ) %>%
      dplyr::collect(),
    tibble::tibble(
      not = c(FALSE, TRUE, NA)
    )
  )
})

test_that("duckdb translation for comparisons works", {
  skip_if_not(has_duckdb_with_substrait())
  tbl <- tibble::tibble(col = c(0, 1, 2, 3, NA))

  expect_identical(
    tbl %>%
      duckdb_substrait_compiler() %>%
      dplyr::transmute(
        gt2 = col > 2,
        gte2 = col >= 2,
        lt2 = col < 2,
        lte2 = col <= 2,
        between_12 = between(col, 1, 2)
      ) %>%
      dplyr::collect(),
    tibble::tibble(
      gt2 = c(FALSE, FALSE, FALSE, TRUE, NA),
      gte2 = c(FALSE, FALSE, TRUE, TRUE, NA),
      lt2 = c(TRUE, TRUE, FALSE, FALSE, NA),
      lte2 = c(TRUE, TRUE, TRUE, FALSE, NA),
      between_12 = c(FALSE, TRUE, TRUE, FALSE, NA)
    )
  )
})

test_that("duckdb translation for arithmetic functions works", {
  skip_if_not(has_duckdb_with_substrait())
  tbl <- tibble::tibble(col = c(1, 2, NA))

  expect_identical(
    tbl %>%
      duckdb_substrait_compiler() %>%
      dplyr::transmute(
        times2 = col * 2,
        div2 = col / 2,
        add2 = col + 2,
        sub2 = col - 2,
        pow2 = col^2
      ) %>%
      dplyr::collect(),
    tibble::tibble(
      times2 = c(2, 4, NA),
      div2 = c(1 / 2, 2 / 2, NA),
      add2 = c(1 + 2, 2 + 2, NA),
      sub2 = c(1 - 2, 2 - 2, NA),
      pow2 = c(1^2, 2^2, NA)
    )
  )
})

test_that("duckdb translation for is.na() works", {
  skip_if_not(has_duckdb_with_substrait())

  expect_identical(
    tibble::tibble(col = c(1, 2, NA)) %>%
      duckdb_substrait_compiler() %>%
      dplyr::filter(is.na(col)) %>%
      dplyr::collect(),
    tibble::tibble(col = NA_real_)
  )
})

test_that("duckdb translation for %in% works", {
  skip_if_not(has_duckdb_with_substrait())

  # case of zero items
  expect_identical(
    tibble::tibble(col = letters) %>%
      duckdb_substrait_compiler() %>%
      dplyr::filter(col %in% c()) %>%
      dplyr::collect(),
    tibble::tibble(col = character())
  )

  # case of one item (translates to ==)
  expect_identical(
    tibble::tibble(col = letters) %>%
      duckdb_substrait_compiler() %>%
      dplyr::filter(col %in% c("d")) %>%
      dplyr::collect(),
    tibble::tibble(col = c("d"))
  )

  # case of n items (translates to == reduced with or)
  expect_identical(
    tibble::tibble(col = letters) %>%
      duckdb_substrait_compiler() %>%
      dplyr::filter(col %in% c("d", "e")) %>%
      dplyr::collect(),
    tibble::tibble(col = c("d", "e"))
  )

  # make sure that a user-provided literal will also work
  expect_identical(
    tibble::tibble(col = letters) %>%
      duckdb_substrait_compiler() %>%
      dplyr::filter(col %in% !!letters) %>%
      dplyr::collect(),
    tibble::tibble(col = letters)
  )

  # ...even if that literal reduces to a scalar literal
  expect_identical(
    tibble::tibble(col = letters) %>%
      duckdb_substrait_compiler() %>%
      dplyr::filter(col %in% !!c("d")) %>%
      dplyr::collect(),
    tibble::tibble(col = "d")
  )

  # make sure that a non-list literal on the rhs errors
  expect_error(
    tibble::tibble(col = letters) %>%
      duckdb_substrait_compiler() %>%
      dplyr::filter(col %in% col),
    "must be a list literal"
  )
})

test_that("duckdb can roundtrip a substrait plan", {
  skip_if_not(has_duckdb_with_substrait())

  plan <- duckdb_get_substrait(
    "SELECT * from mtcars",
    tables = list(mtcars = mtcars)
  )

  # not sure why the table name doesn't come through here
  plan <- rel_tree_modify(plan, "substrait_ReadRel_NamedTable", function(x) {
    x$names <- "mtcars"
    x
  })

  expect_equal(
    duckdb_from_substrait(plan, tables = list(mtcars = mtcars)),
    mtcars,
    ignore_attr = TRUE
  )
})

test_that("duckdb raises error for empty projection", {
  skip_if_not(has_duckdb_with_substrait())
  tbl <- tibble::tibble(col = c(1, 2, NA))

  expect_error(
    tbl %>%
      duckdb_substrait_compiler() %>%
      substrait_select(),
    "Column list must not be empty"
  )
})


test_that("duckdb translation for != works", {
  skip_if_not(has_duckdb_with_substrait())

  expect_equal(
    example_data[1:5, "dbl"] %>%
      duckdb_substrait_compiler() %>%
      substrait_filter(dbl != 0) %>%
      dplyr::collect(),
    tibble::tibble(dbl = c(-999, -99, -9, 9))
  )
})

test_that("duckdb translation for == works", {
  skip_if_not(has_duckdb_with_substrait())

  expect_equal(
    example_data[1:5, "dbl"] %>%
      duckdb_substrait_compiler() %>%
      substrait_filter(dbl == 0) %>%
      dplyr::collect(),
    tibble::tibble(dbl = 0)
  )
})

test_that("duckdb translation for < works", {
  skip_if_not(has_duckdb_with_substrait())

  expect_equal(
    example_data[1:5, "dbl"] %>%
      duckdb_substrait_compiler() %>%
      substrait_filter(dbl < 0) %>%
      dplyr::collect(),
    tibble::tibble(dbl = c(-999, -99, -9))
  )
})

test_that("duckdb translation for > works", {
  skip_if_not(has_duckdb_with_substrait())

  expect_equal(
    example_data[1:5, "dbl"] %>%
      duckdb_substrait_compiler() %>%
      substrait_filter(dbl > 0) %>%
      dplyr::collect(),
    tibble::tibble(dbl = 9)
  )
})

test_that("duckdb translation for <= works", {
  skip_if_not(has_duckdb_with_substrait())

  expect_equal(
    example_data[1:5, "dbl"] %>%
      duckdb_substrait_compiler() %>%
      substrait_filter(dbl <= 0) %>%
      dplyr::collect(),
    tibble::tibble(dbl = c(-999, -99, -9, 0))
  )
})

test_that("duckdb translation for >= works", {
  skip_if_not(has_duckdb_with_substrait())

  expect_equal(
    example_data[1:5, "dbl"] %>%
      duckdb_substrait_compiler() %>%
      substrait_filter(dbl >= 0) %>%
      dplyr::collect(),
    tibble::tibble(dbl = c(0, 9))
  )
})

test_that("duckdb translation for grepl works", {
  skip_if_not(has_duckdb_with_substrait())

  expect_equal(
    tibble::tibble(x = c("cat", "bat", "mouse")) %>%
      duckdb_substrait_compiler() %>%
      substrait_filter(grepl("a", x)) %>%
      dplyr::collect(),
    tibble::tibble(
      x = c("cat", "bat")
    )
  )
})

test_that("duckdb translation for if_else() works", {
  skip_if_not(has_duckdb_with_substrait())

  expect_equal(
    tibble::tibble(dbl = c(-999, -99, -9, 0, 9)) %>%
      duckdb_substrait_compiler() %>%
      substrait_project(dbl, gt_five = if_else(dbl > 5, "over", "under")) %>%
      dplyr::collect(),
    tibble::tibble(
      dbl = c(-999, -99, -9, 0, 9),
      gt_five = c("under", "under", "under", "under", "over")
    )
  )
})

test_that("duckdb translation for n_distinct works", {
  skip_if_not(has_duckdb_with_substrait())

  suppressWarnings(
    expect_equal(
      tibble::tibble(x = c(1:5, 1:3)) %>%
        duckdb_substrait_compiler() %>%
        substrait_aggregate(n = n_distinct(x, na.rm = TRUE)) %>%
        dplyr::collect(),
      tibble::tibble(
        n = 5
      )
    ),
    classes = "substrait.n_distinct.approximate"
  )
})

test_that("duckdb translation for year() works", {
  skip_if_not(has_duckdb_with_substrait())

  expect_equal(
    tibble::tibble(x = as.Date("1987-10-09")) %>%
      duckdb_substrait_compiler() %>%
      substrait_project(x, year = year(x)) %>%
      dplyr::collect(),
    tibble::tibble(x = as.Date("1987-10-09"), year = 1987)
  )
}
test_that("duckdb translation for round works", {
  skip_if_not(has_duckdb_with_substrait())

  expect_equal(
    tibble::tibble(x = c(1, 2.34, 3.456, 4.5)) %>%
      duckdb_substrait_compiler() %>%
      substrait_project(x, y = round(x)) %>%
      dplyr::collect(),
    tibble::tibble(
      y = c(1, 2, 3, 5)
    )
  )

  expect_equal(
    tibble::tibble(x = c(1, 2.34, 3.456, 4.5)) %>%
      duckdb_substrait_compiler() %>%
      substrait_project(x, y = ceiling(x)) %>%
      dplyr::collect(),
    tibble::tibble(
      x = c(1, 2.34, 3.456, 4.5),
      y = c(1, 3, 4, 5)
    )
  )

  expect_equal(
    tibble::tibble(x = c(1, 2.34, 3.456, 4.5)) %>%
      duckdb_substrait_compiler() %>%
      substrait_project(x, y = floor(x)) %>%
      dplyr::collect(),
    tibble::tibble(
      x = c(1, 2.34, 3.456, 4.5),
      y = c(1, 2, 3, 4)
    )
  )

})
