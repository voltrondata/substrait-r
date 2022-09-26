withr::local_options(list(
  arrow.summarise.sort = TRUE,
  rlib_warning_verbosity = "verbose",
  # This prevents the warning in `summarize()` about having grouped output without
  # also specifying what to do with `.groups`
  dplyr.summarise.inform = FALSE
))

skip_if_not(has_arrow_with_substrait())

library(dplyr, warn.conflicts = FALSE)
library(stringr)

test_that("Can aggregate", {
  skip("na.rm argument causes error: https://github.com/voltrondata/substrait-r/issues/141")

  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      summarize(total = sum(int)) %>%
      collect(),
    example_data
  )

  skip("https://github.com/voltrondata/substrait-r/issues/142")

  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      summarize(total = sum(int, na.rm = TRUE)) %>%
      collect(),
    example_data
  )
})

test_that("Group by sum on dataset", {
  skip("na.rm argument causes error: https://github.com/voltrondata/substrait-r/issues/141")

  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl) %>%
      summarize(total = sum(int, na.rm = TRUE)) %>%
      collect(),
    example_data
  )

  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl) %>%
      summarize(total = sum(int * 4, na.rm = TRUE)) %>%
      collect(),
    example_data
  )

  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl) %>%
      summarize(total = sum(int)) %>%
      collect(),
    example_data,
  )
})

test_that("Group by mean on dataset", {
  skip("na.rm argument causes error: https://github.com/voltrondata/substrait-r/issues/141")
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl) %>%
      summarize(mean = mean(int, na.rm = TRUE)) %>%
      collect(),
    example_data
  )

  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl) %>%
      summarize(mean = mean(int, na.rm = FALSE)) %>%
      collect(),
    example_data
  )
})

test_that("Group by sd on dataset", {
  skip("na.rm argument causes error: https://github.com/voltrondata/substrait-r/issues/141")
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl) %>%
      summarize(sd = sd(int, na.rm = TRUE)) %>%
      collect(),
    example_data
  )

  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl) %>%
      summarize(sd = sd(int, na.rm = FALSE)) %>%
      collect(),
    example_data
  )
})

test_that("Group by var on dataset", {
  skip("na.rm argument causes error: https://github.com/voltrondata/substrait-r/issues/141")
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl) %>%
      summarize(var = var(int, na.rm = TRUE)) %>%
      collect(),
    example_data
  )

  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl) %>%
      summarize(var = var(int, na.rm = FALSE)) %>%
      collect(),
    example_data
  )
})

test_that("n()", {
  skip("n() not implemented: https://github.com/voltrondata/substrait-r/issues/143")

  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      summarize(counts = n()) %>%
      collect(),
    example_data
  )

  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl) %>%
      summarize(counts = n()) %>%
      arrange(lgl) %>%
      collect(),
    example_data
  )
})

test_that("Group by any/all", {
  skip("any/all not implemented https://github.com/voltrondata/substrait-r/issues/144")
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl) %>%
      summarize(any(lgl, na.rm = TRUE)) %>%
      collect(),
    example_data
  )
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl) %>%
      summarize(all(lgl, na.rm = TRUE)) %>%
      collect(),
    example_data
  )
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl) %>%
      summarize(any(lgl, na.rm = FALSE)) %>%
      collect(),
    example_data
  )
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl) %>%
      summarize(all(lgl, na.rm = FALSE)) %>%
      collect(),
    example_data
  )

  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      mutate(has_words = nchar(verses) < 0) %>%
      group_by(lgl) %>%
      summarize(any(has_words, na.rm = TRUE)) %>%
      collect(),
    example_data
  )
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      mutate(has_words = nchar(verses) < 0) %>%
      group_by(lgl) %>%
      summarize(all(has_words, na.rm = TRUE)) %>%
      collect(),
    example_data
  )
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl) %>%
      summarize(has_words = all(nchar(verses) < 0, na.rm = TRUE)) %>%
      collect(),
    example_data
  )
})

test_that("n_distinct() on dataset", {
  skip("n_distinct() not implemented: https://github.com/voltrondata/substrait-r/issues/145")

  # With groupby
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl) %>%
      summarize(distinct = n_distinct(lgl, na.rm = FALSE)) %>%
      collect(),
    example_data
  )
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl) %>%
      summarize(distinct = n_distinct(lgl, na.rm = TRUE)) %>%
      collect(),
    example_data
  )
  # Without groupby
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      summarize(distinct = n_distinct(lgl, na.rm = FALSE)) %>%
      collect(),
    example_data
  )
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      summarize(distinct = n_distinct(lgl, na.rm = TRUE)) %>%
      collect(),
    example_data
  )

  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      summarize(distinct = n_distinct(int, lgl)) %>%
      collect(),
    example_data,
    warning = "Multiple arguments"
  )
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl) %>%
      summarize(distinct = n_distinct(int, lgl)) %>%
      collect(),
    example_data,
    warning = "Multiple arguments"
  )
})

test_that("median()", {
  skip("median not yet implemented: https://github.com/voltrondata/substrait-r/issues/146")

  # When medians are integer-valued, stats::median() sometimes returns output of
  # type integer, whereas whereas the Arrow approx_median kernels always return
  # output of type float64. The calls to median(int, ...) in the tests below
  # are enclosed in as.double() to work around this known difference.

  # with groups
  suppressWarnings(
    compare_dplyr_binding(
      .input %>%
        group_by(lgl) %>%
        summarize(
          med_dbl = median(dbl),
          med_int = as.double(median(int)),
          med_dbl_narmf = median(dbl, FALSE),
          med_int_narmf = as.double(median(int, na.rm = FALSE)),
          med_dbl_narmt = median(dbl, na.rm = TRUE),
          med_int_narmt = as.double(median(int, TRUE))
        ) %>%
        arrange(lgl) %>%
        collect(),
      example_data,
      warning = "median\\(\\) currently returns an approximate median in Arrow"
    ),
    classes = "arrow.median.approximate"
  )
  # without groups, with na.rm = TRUE
  suppressWarnings(
    compare_dplyr_binding(
      .input %>%
        summarize(
          med_dbl_narmt = median(dbl, na.rm = TRUE),
          med_int_narmt = as.double(median(int, TRUE))
        ) %>%
        collect(),
      example_data,
      warning = "median\\(\\) currently returns an approximate median in Arrow"
    ),
    classes = "arrow.median.approximate"
  )
  # without groups, with na.rm = FALSE (the default)
  suppressWarnings(
    compare_dplyr_binding(
      .input %>%
        summarize(
          med_dbl = median(dbl),
          med_int = as.double(median(int)),
          med_dbl_narmf = median(dbl, FALSE),
          med_int_narmf = as.double(median(int, na.rm = FALSE))
        ) %>%
        collect(),
      example_data,
      warning = "median\\(\\) currently returns an approximate median in Arrow"
    ),
    classes = "arrow.median.approximate"
  )
})

test_that("quantile()", {
  skip("quantile not yet implemented: https://github.com/voltrondata/substrait-r/issues/147")
  # The default method for stats::quantile() throws an error when na.rm = FALSE
  # and the input contains NA or NaN, whereas the Arrow tdigest kernels return
  # null in this situation. To work around this known difference, the tests
  # below always use na.rm = TRUE when the data contains NA or NaN.

  # The default method for stats::quantile() has an argument `names` that
  # controls whether the result has a names attribute. It defaults to
  # names = TRUE. With Arrow, it is not possible to give the result a names
  # attribute, so the quantile() binding in Arrow does not accept a `names`
  # argument. Differences in this names attribute cause compare_dplyr_binding() to
  # report that the objects are not equal, so we do not use compare_dplyr_binding()
  # in the tests below.

  # The tests below all use probs = 0.5 because other values cause differences
  # between the exact quantiles returned by R and the approximate quantiles
  # returned by Arrow.

  # When quantiles are integer-valued, stats::quantile() sometimes returns
  # output of type integer, whereas whereas the Arrow tdigest kernels always
  # return output of type float64. The calls to quantile(int, ...) in the tests
  # below are enclosed in as.double() to work around this known difference.

  # with groups
  suppressWarnings(
    expect_warning(
      expect_equal(
        example_data %>%
          group_by(lgl) %>%
          summarize(
            q_dbl = quantile(dbl, probs = 0.5, na.rm = TRUE, names = FALSE),
            q_int = as.double(
              quantile(int, probs = 0.5, na.rm = TRUE, names = FALSE)
            )
          ) %>%
          arrange(lgl),
        Table$create(example_data) %>%
          group_by(lgl) %>%
          summarize(
            q_dbl = quantile(dbl, probs = 0.5, na.rm = TRUE),
            q_int = as.double(quantile(int, probs = 0.5, na.rm = TRUE))
          ) %>%
          arrange(lgl) %>%
          collect()
      ),
      "quantile() currently returns an approximate quantile in Arrow",
      fixed = TRUE
    ),
    classes = "arrow.quantile.approximate"
  )

  # without groups
  suppressWarnings(
    expect_warning(
      expect_equal(
        example_data %>%
          summarize(
            q_dbl = quantile(dbl, probs = 0.5, na.rm = TRUE, names = FALSE),
            q_int = as.double(
              quantile(int, probs = 0.5, na.rm = TRUE, names = FALSE)
            )
          ),
        Table$create(example_data) %>%
          summarize(
            q_dbl = quantile(dbl, probs = 0.5, na.rm = TRUE),
            q_int = as.double(quantile(int, probs = 0.5, na.rm = TRUE))
          ) %>%
          collect()
      ),
      "quantile() currently returns an approximate quantile in Arrow",
      fixed = TRUE
    ),
    classes = "arrow.quantile.approximate"
  )

  # with missing values and na.rm = FALSE
  suppressWarnings(
    expect_warning(
      expect_equal(
        tibble(
          q_dbl = NA_real_,
          q_int = NA_real_
        ),
        Table$create(example_data) %>%
          summarize(
            q_dbl = quantile(dbl, probs = 0.5, na.rm = FALSE),
            q_int = as.double(quantile(int, probs = 0.5, na.rm = FALSE))
          ) %>%
          collect()
      ),
      "quantile() currently returns an approximate quantile in Arrow",
      fixed = TRUE
    ),
    classes = "arrow.quantile.approximate"
  )

  # with a vector of 2+ probs
  expect_warning(
    Table$create(example_data) %>%
      summarize(q = quantile(dbl, probs = c(0.2, 0.8), na.rm = TRUE)),
    "quantile() with length(probs) != 1 not supported in Arrow",
    fixed = TRUE
  )
})

test_that("summarize() with min() and max()", {
  skip("min and max not implemented yet: https://github.com/voltrondata/substrait-r/issues/148")

  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      select(int, chr) %>%
      filter(int > 5) %>% # this filters out the NAs in `int`
      summarize(min_int = min(int), max_int = max(int)) %>%
      collect(),
    example_data,
  )
  compare_dplyr_binding(
    # skip("min and max not implemented yet: https://github.com/voltrondata/substrait-r/issues/148")
    engine = "duckdb",
    .input %>%
      select(int, chr) %>%
      filter(int > 5) %>% # this filters out the NAs in `int`
      summarize(
        min_int = min(int + 4) / 2,
        max_int = 3 / max(42 - int)
      ) %>%
      collect(),
    example_data,
  )

  skip("doesn't work with NAs: https://github.com/voltrondata/substrait-r/issues/149")
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      select(int, chr) %>%
      summarize(min_int = min(int), max_int = max(int)) %>%
      collect(),
    example_data,
  )

  skip("Error calling min on two integer columns: https://github.com/voltrondata/substrait-r/issues/151")
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      select(int) %>%
      summarize(
        min_int = min(int, na.rm = TRUE),
        max_int = max(int, na.rm = TRUE)
      ) %>%
      collect(),
    example_data,
  )

  skip("max on int and double doesn't work: https://github.com/voltrondata/substrait-r/issues/150")
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      select(dbl, int) %>%
      summarize(
        min_int = -min(log(ceiling(dbl)), na.rm = TRUE),
        max_int = log(max(as.double(int), na.rm = TRUE))
      ) %>%
      collect(),
    example_data,
  )

  # multiple dots arguments to min(), max() not supported
  skip("error calling max on diff types: https://github.com/voltrondata/substrait-r/issues/150")
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      summarize(min_mult = min(dbl, int)) %>%
      collect(),
    example_data,
    warning = "Multiple arguments to min\\(\\) not supported in Arrow"
  )
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      select(int, dbl, dbl2) %>%
      summarize(max_mult = max(int, dbl, dbl2)) %>%
      collect(),
    example_data,
    warning = "Multiple arguments to max\\(\\) not supported in Arrow"
  )

  # min(logical) or max(logical) yields integer in R
  # min(Boolean) or max(Boolean) yields Boolean in Arrow
  skip("as.logical not implemented: https://github.com/voltrondata/substrait-r/issues/152")
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      select(lgl) %>%
      summarize(
        max_lgl = as.logical(max(lgl, na.rm = TRUE)),
        min_lgl = as.logical(min(lgl, na.rm = TRUE))
      ) %>%
      collect(),
    example_data,
  )
})

test_that("min() and max() on character strings", {
  skip("error calling max on diff types: https://github.com/voltrondata/substrait-r/issues/150")
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      summarize(
        min_chr = min(chr, na.rm = TRUE),
        max_chr = max(chr, na.rm = TRUE)
      ) %>%
      collect(),
    example_data,
  )
  skip("Strings not supported by hash_min_max (ARROW-13988)")
  compare_dplyr_binding(
    .input %>%
      group_by(fct) %>%
      summarize(
        min_chr = min(chr, na.rm = TRUE),
        max_chr = max(chr, na.rm = TRUE)
      ) %>%
      collect(),
    example_data,
  )
})

test_that("summarise() with !!sym()", {
  skip("any/all not implemented: https://github.com/voltrondata/substrait-r/issues/144")

  test_chr_col <- "int"
  test_dbl_col <- "dbl"
  test_lgl_col <- "lgl"
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(false) %>%
      summarise(
        sum = sum(!!sym(test_dbl_col)),
        any = any(!!sym(test_lgl_col)),
        all = all(!!sym(test_lgl_col)),
        mean = mean(!!sym(test_dbl_col)),
        sd = sd(!!sym(test_dbl_col)),
        var = var(!!sym(test_dbl_col)),
        n_distinct = n_distinct(!!sym(test_chr_col)),
        min = min(!!sym(test_dbl_col)),
        max = max(!!sym(test_dbl_col))
      ) %>%
      collect(),
    example_data
  )
})

test_that("Filter and aggregate", {
  skip("sum() doesn't work with na.rm = TRUE: https://github.com/voltrondata/substrait-r/issues/141")

  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      filter(lgl == 2) %>%
      summarize(total = sum(int, na.rm = TRUE)) %>%
      collect(),
    example_data
  )

  compare_dplyr_binding(
    .input %>%
      filter(int > 5) %>%
      summarize(total = sum(int, na.rm = TRUE)) %>%
      collect(),
    example_data
  )

  compare_dplyr_binding(
    .input %>%
      filter(lgl == 2) %>%
      group_by(lgl) %>%
      summarize(total = sum(int, na.rm = TRUE)) %>%
      collect(),
    example_data
  )

  compare_dplyr_binding(
    .input %>%
      filter(int > 5) %>%
      group_by(lgl) %>%
      summarize(total = sum(int, na.rm = TRUE)) %>%
      collect(),
    example_data
  )
})

test_that("Group by edge cases", {
  skip("sum() doesn't work with na.rm = TRUE: https://github.com/voltrondata/substrait-r/issues/141")

  compare_dplyr_binding(
    .input %>%
      group_by(lgl * 2) %>%
      summarize(total = sum(int, na.rm = TRUE)) %>%
      collect(),
    example_data
  )

  compare_dplyr_binding(
    .input %>%
      group_by(alt = lgl * 2) %>%
      summarize(total = sum(int, na.rm = TRUE)) %>%
      collect(),
    example_data
  )
})

test_that("Do things after summarize", {
  skip("sum() doesn't work with na.rm = TRUE: https://github.com/voltrondata/substrait-r/issues/141")

  group2_sum <- example_data %>%
    group_by(lgl) %>%
    filter(int > 5) %>%
    summarize(total = sum(int, na.rm = TRUE)) %>%
    pull() %>%
    tail(1)

  compare_dplyr_binding(
    .input %>%
      group_by(lgl) %>%
      filter(int > 5) %>%
      summarize(total = sum(int, na.rm = TRUE)) %>%
      filter(total == group2_sum) %>%
      mutate(extra = total * 5) %>%
      collect(),
    example_data
  )

  compare_dplyr_binding(
    .input %>%
      filter(dbl > 2) %>%
      select(chr, int, lgl) %>%
      mutate(twice = int * 2L) %>%
      group_by(lgl) %>%
      summarize(
        count = n(),
        total = sum(twice, na.rm = TRUE)
      ) %>%
      mutate(mean = total / count) %>%
      collect(),
    example_data
  )
})

test_that("Expressions on aggregations", {
  skip("any not implemented: https://github.com/voltrondata/substrait-r/issues/144")
  # This is what it effectively is
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl) %>%
      summarize(
        any = any(lgl),
        all = all(lgl)
      ) %>%
      ungroup() %>% # TODO: loosen the restriction on mutate after group_by
      mutate(some = any & !all) %>%
      select(lgl, some) %>%
      collect(),
    example_data
  )
  # More concisely:
  compare_dplyr_binding(
    .input %>%
      group_by(lgl) %>%
      summarize(any(lgl) & !all(lgl)) %>%
      collect(),
    example_data
  )

  # Save one of the aggregates first
  compare_dplyr_binding(
    .input %>%
      group_by(lgl) %>%
      summarize(
        any_lgl = any(lgl),
        some = any_lgl & !all(lgl)
      ) %>%
      collect(),
    example_data
  )

  # Make sure order of columns in result is correct
  compare_dplyr_binding(
    .input %>%
      group_by(lgl) %>%
      summarize(
        any_lgl = any(lgl),
        some = any_lgl & !all(lgl),
        n()
      ) %>%
      collect(),
    example_data
  )

  # Aggregates on aggregates are not supported
  expect_warning(
    record_batch(example_data) %>% summarise(any(any(lgl))),
    paste(
      "Aggregate within aggregate expression",
      "any\\(any\\(lgl\\)\\) not supported in Arrow"
    )
  )

  # Check aggregates on aggeregates with more complex calls
  expect_warning(
    record_batch(example_data) %>% summarise(any(any(!lgl))),
    paste(
      "Aggregate within aggregate expression",
      "any\\(any\\(!lgl\\)\\) not supported in Arrow"
    )
  )
  expect_warning(
    record_batch(example_data) %>% summarise(!any(any(lgl))),
    paste(
      "Aggregate within aggregate expression",
      "any\\(any\\(lgl\\)\\) not supported in Arrow"
    )
  )
})

test_that("Summarize with 0 arguments", {
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl) %>%
      summarize() %>%
      collect(),
    example_data
  )
})

test_that("Not (yet) supported: implicit join", {
  skip("complex expressions not supported: https://github.com/voltrondata/substrait-r/issues/155")
  withr::local_options(list(arrow.debug = TRUE))
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl) %>%
      summarize(
        sum((dbl - mean(dbl))^2)
      ) %>%
      collect(),
    example_data,
    warning = paste(
      "Aggregate within aggregate expression sum\\(\\(dbl - mean\\(dbl\\)\\)\\^2\\)",
      "not supported in Arrow; pulling data into R"
    )
  )
  compare_dplyr_binding(
    .input %>%
      group_by(lgl) %>%
      summarize(
        sum(dbl - mean(dbl))
      ) %>%
      collect(),
    example_data,
    warning = paste(
      "Aggregate within aggregate expression sum\\(dbl - mean\\(dbl\\)\\)",
      "not supported in Arrow; pulling data into R"
    )
  )
  compare_dplyr_binding(
    .input %>%
      group_by(lgl) %>%
      summarize(
        sqrt(sum((dbl - mean(dbl))^2) / (n() - 1L))
      ) %>%
      collect(),
    example_data,
    warning = paste(
      "Aggregate within aggregate expression sum\\(\\(dbl - mean\\(dbl\\)\\)\\^2\\)",
      "not supported in Arrow; pulling data into R"
    )
  )

  compare_dplyr_binding(
    .input %>%
      group_by(lgl) %>%
      summarize(
        dbl - mean(dbl)
      ) %>%
      collect(),
    example_data,
    warning = paste(
      "Expression dbl - mean\\(dbl\\) is not an aggregate expression",
      "or is not supported in Arrow; pulling data into R"
    )
  )

  compare_dplyr_binding(
    .input %>%
      group_by(lgl) %>%
      summarize(
        dbl
      ) %>%
      collect(),
    example_data,
    warning = paste(
      "Expression dbl is not an aggregate expression",
      "or is not supported in Arrow; pulling data into R"
    )
  )

  # This one could possibly be supported--in mutate()
  compare_dplyr_binding(
    .input %>%
      group_by(lgl) %>%
      summarize(
        dbl - int
      ) %>%
      collect(),
    example_data,
    warning = paste(
      "Expression dbl - int is not an aggregate expression",
      "or is not supported in Arrow; pulling data into R"
    )
  )
})

test_that(".groups argument", {
  skip("n() not yet supported: https://github.com/voltrondata/substrait-r/issues/143")
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl, int < 6) %>%
      summarize(count = n()) %>%
      collect(),
    example_data
  )
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(lgl, int < 6) %>%
      summarize(count = n(), .groups = "drop_last") %>%
      collect(),
    example_data
  )
  compare_dplyr_binding(
    .input %>%
      group_by(lgl, int < 6) %>%
      summarize(count = n(), .groups = "keep") %>%
      collect(),
    example_data
  )
  compare_dplyr_binding(
    .input %>%
      group_by(lgl, int < 6) %>%
      summarize(count = n(), .groups = "drop") %>%
      collect(),
    example_data
  )
  compare_dplyr_binding(
    .input %>%
      group_by(lgl, int < 6) %>%
      summarize(count = n(), .groups = "rowwise") %>%
      collect(),
    example_data,
    warning = TRUE
  )

  # abandon_ship() raises the warning, then dplyr itself errors
  # This isn't ideal but it's fine and won't be an issue on Datasets
  expect_error(
    expect_warning(
      Table$create(example_data) %>%
        group_by(lgl, int < 6) %>%
        summarize(count = n(), .groups = "NOTVALID"),
      "Invalid .groups argument"
    ),
    "NOTVALID"
  )
})

test_that("summarize() handles group_by .drop", {
  # Error: Type error: Sorting not supported for type dictionary<values=string, indices=int8, ordered=0>
  withr::local_options(list(arrow.summarise.sort = FALSE))

  skip("factors not yet supported: https://github.com/voltrondata/substrait-r/issues/138")

  example_data <- tibble(
    x = 1:10,
    y = factor(rep(c("a", "c"), each = 5), levels = c("a", "b", "c"))
  )
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(y) %>%
      count() %>%
      collect() %>%
      arrange(y),
    example_data
  )
  # Not supported: check message
  compare_dplyr_binding(
    .input %>%
      group_by(y, .drop = FALSE) %>%
      count() %>%
      collect() %>%
      # Because it's not supported, we have to filter out the (empty) row
      # that dplyr keeps, just so we test equal (otherwise)
      filter(y != "b") %>%
      arrange(y),
    example_data,
    warning = ".drop = FALSE currently not supported in Arrow aggregation"
  )

  # But this is ok because there is no factor group
  compare_dplyr_binding(
    .input %>%
      group_by(y, .drop = FALSE) %>%
      count() %>%
      collect() %>%
      arrange(y),
    tibble(
      x = 1:10,
      y = rep(c("a", "c"), each = 5)
    )
  )
})

test_that("summarise() passes through type information for temporary columns", {
  # applies to ifelse and case_when(), in which argument types are checked
  # within a translated function (previously this failed because the appropriate
  # schema was not available for n() > 1, mean(y), and mean(z))

  skip("https://github.com/voltrondata/substrait-r/issues/156")
  compare_dplyr_binding(
    engine = "duckdb",
    .input %>%
      group_by(x) %>%
      summarise(r = if_else(n() > 1, mean(y), mean(z))) %>%
      collect(),
    tibble(
      x = c(0, 1, 1),
      y = c(2, 3, 5),
      z = c(8, 13, 21)
    )
  )
})

test_that("summarise() can handle scalars and literal values", {
  skip("https://github.com/voltrondata/substrait-r/issues/153")

  some_scalar_value <- 2L

  compare_dplyr_binding(
    engine = "duckdb",
    .input %>% summarise(y = 1L) %>% collect(),
    example_data
  )

  compare_dplyr_binding(
    .input %>% summarise(y = some_scalar_value) %>% collect(),
    example_data
  )

  compare_dplyr_binding(
    .input %>% summarise(y = !!some_scalar_value) %>% collect(),
    example_data
  )
})
