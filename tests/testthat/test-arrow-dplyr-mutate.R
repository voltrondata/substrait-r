library(dplyr, warn.conflicts = FALSE)
library(arrow)
skip_if_not(has_arrow_with_substrait())

library(stringr)

test_that("basic mutate", {
  compare_dplyr_binding(
    #   skip("arithmetic functions not yet implemented: https://github.com/voltrondata/substrait-r/issues/20")
    engine = "duckdb",
    .input %>%
      mutate(newcol = some_negative -  2L) %>%
      collect(),
    example_data
  )

  compare_dplyr_binding(
    #   skip("arithmetic functions not yet implemented: https://github.com/voltrondata/substrait-r/issues/20")
    engine = "duckdb",
    .input %>%
      mutate(newcol = dbl + 6L) %>%
      collect(),
    example_data
  )

  skip("https://github.com/voltrondata/substrait-r/issues/54")
  compare_dplyr_binding(
    .input %>%
      mutate(int = int + 6L) %>%
      collect(),
    example_data
  )
})

test_that("mutate after select", {
  skip("https://github.com/voltrondata/substrait-r/issues/55")
  compare_dplyr_binding(
    .input %>%
      select(int, chr) %>%
      mutate(int2 = int + 6L) %>%
      collect(),
    example_data
  )
})

test_that("mutate() with NULL inputs", {
  skip("https://github.com/voltrondata/substrait-r/issues/56")

  compare_dplyr_binding(
    .input %>%
      mutate(int2 = NULL) %>%
      collect(),
    example_data
  )
})

test_that("empty mutate()", {
  compare_dplyr_binding(
    .input %>%
      mutate() %>%
      collect(),
    example_data
  )
})

test_that("transmute", {
  compare_dplyr_binding(
    # skip("arithmetic functions not yet implemented: https://github.com/voltrondata/substrait-r/issues/20")
    engine = "duckdb",
    .input %>%
      transmute(new_col = some_negative -  2L) %>%
      collect(),
    example_data
  )
})

test_that("transmute respect bespoke dplyr implementation", {
  # see: https://github.com/tidyverse/dplyr/issues/6086
  compare_dplyr_binding(
    .input %>%
      transmute(dbl, new_col = some_negative + 6L) %>%
      collect(),
    example_data
  )
})

test_that("transmute() with NULL inputs", {
  skip("https://github.com/voltrondata/substrait-r/issues/56")
  compare_dplyr_binding(
    .input %>%
      transmute(int = NULL) %>%
      collect(),
    example_data
  )
})

test_that("empty transmute()", {
  skip("https://github.com/voltrondata/substrait-r/issues/51")
  compare_dplyr_binding(
    .input %>%
      transmute() %>%
      collect(),
    example_data
  )
})

test_that("transmute with unnamed expressions", {
  compare_dplyr_binding(
    .input %>%
      transmute(
        int, # bare column name
        int + 1 # expression
      ) %>%
      collect(),
    example_data
  )
})

test_that("transmute() with unsupported arguments", {
  skip("https://github.com/voltrondata/substrait-r/issues/58")

  expect_error(
    example_data %>%
      arrow_substrait_compiler() %>%
      transmute(int = int + 42L, .keep = "all"),
    "`transmute()` does not support the `.keep` argument",
    fixed = TRUE
  )
  expect_error(
    example_data %>%
      arrow_substrait_compiler() %>%
      transmute(int = int + 42L, .before = lgl),
    "`transmute()` does not support the `.before` argument",
    fixed = TRUE
  )
  expect_error(
    example_data %>%
      arrow_substrait_compiler() %>%
      transmute(int = int + 42L, .after = chr),
    "`transmute()` does not support the `.after` argument",
    fixed = TRUE
  )
})

test_that("mutate and refer to previous mutants", {
  compare_dplyr_binding(
    .input %>%
      mutate(
        int10 = int + 10
      ) %>%
      select(int10) %>%
      collect(),
    example_data
  )
})

test_that("mutate with .data pronoun", {
  compare_dplyr_binding(
    .input %>%
      mutate(
        int10 = .data$int + 10
      ) %>%
      collect(),
    example_data
  )
})

test_that("mutate with unnamed expressions", {
  compare_dplyr_binding(
    .input %>%
      mutate(
        int + 1 # expression
      ) %>%
      collect(),
    example_data
  )
})

test_that("mutate with reassigning same name", {
  skip("https://github.com/voltrondata/substrait-r/issues/59")
  compare_dplyr_binding(
    .input %>%
      transmute(
        new = lgl,
        new = chr
      ) %>%
      collect(),
    example_data
  )
})

test_that("mutate with single value for recycling", {
  compare_dplyr_binding(
    .input %>%
      mutate(
        dr_bronner = 1 # ALL ONE!
      ) %>%
      collect(),
    example_data
  )
})

test_that("dplyr::mutate's examples", {
  skip("https://github.com/voltrondata/substrait-r/issues/60")

  # Newly created variables are available immediately
  compare_dplyr_binding(
    .input %>%
      select(name, mass) %>%
      mutate(
        mass2 = mass * 2,
        mass2_squared = mass2 * mass2
      ) %>%
      collect(),
    starwars # this is a test tibble that ships with dplyr
  )

  # As well as adding new variables, you can use mutate() to
  # remove variables and modify existing variables.
  compare_dplyr_binding(
    .input %>%
      select(name, height, mass, homeworld) %>%
      mutate(
        mass = NULL,
        height = height * 0.0328084 # convert to feet
      ) %>%
      collect(),
    starwars
  )
})

test_that("window functions", {
  skip("group_by not yet implemented: https://github.com/voltrondata/substrait-r/issues/28")
  # Grouping ----------------------------------------
  # The mutate operation may yield different results on grouped
  # tibbles because the expressions are computed within groups.
  # The following normalises `mass` by the global average:
  compare_dplyr_binding(
    .input %>%
      select(name, mass, species) %>%
      mutate(mass_norm = mass / mean(mass, na.rm = TRUE)) %>%
      collect(),
    starwars # ,
    # warning = "window function"
  )
})

test_that("mutate() with keep argument", {
  skip("https://github.com/voltrondata/substrait-r/issues/66")

  # Experimental: You can override with `.keep`
  df <- tibble(x = 1, y = 2, a = "a", b = "b")
  compare_dplyr_binding(
    .input %>% mutate(z = x + y, .keep = "all") %>% collect(), # the default
    df
  )
  #> # A tibble: 1 x 5
  #>       x     y a     b         z
  #>   <dbl> <dbl> <chr> <chr> <dbl>
  #> 1     1     2 a     b         3
  compare_dplyr_binding(
    .input %>% mutate(z = x + y, .keep = "used") %>% collect(),
    df
  )
  #> # A tibble: 1 x 3
  #>       x     y     z
  #>   <dbl> <dbl> <dbl>
  #> 1     1     2     3
  compare_dplyr_binding(
    .input %>% mutate(z = x + y, .keep = "unused") %>% collect(),
    df
  )
  #> # A tibble: 1 x 3
  #>   a     b         z
  #>   <chr> <chr> <dbl>
  #> 1 a     b         3
  compare_dplyr_binding(
    .input %>% mutate(z = x + y, x, .keep = "none") %>% collect(),
    df
  )
  #> # A tibble: 1 × 2
  #>       x     z
  #>   <dbl> <dbl>
  #> 1     1     3
})

test_that("mutate() with .before and .after", {
  skip("https://github.com/voltrondata/substrait-r/issues/65")

  # `.before` and `.after` experimental args
  df <- tibble(x = 1, y = 2)
  compare_dplyr_binding(
    .input %>% mutate(z = x + y) %>% collect(),
    df
  )
  #> # A tibble: 1 x 3
  #>       x     y     z
  #>   <dbl> <dbl> <dbl>
  #> 1     1     2     3

  compare_dplyr_binding(
    .input %>% mutate(z = x + y, .before = 1) %>% collect(),
    df
  )
  #> # A tibble: 1 x 3
  #>       z     x     y
  #>   <dbl> <dbl> <dbl>
  #> 1     3     1     2
  compare_dplyr_binding(
    .input %>% mutate(z = x + y, .after = x) %>% collect(),
    df
  )
  #> # A tibble: 1 x 3
  #>       x     z     y
  #>   <dbl> <dbl> <dbl>
  #> 1     1     3     2
})

test_that("across()", {
  skip("https://github.com/voltrondata/substrait-r/issues/64")
  compare_dplyr_binding(
    .input %>%
      select(int, dbl, lgl) %>%
      mutate(across(int, as.character)) %>%
      collect(),
    example_data
  )
})

test_that("group_by() followed by mutate()", {
  skip("group_by not yet implemented: https://github.com/voltrondata/substrait-r/issues/28")
  compare_dplyr_binding(
    .input %>%
      select(name, mass, homeworld) %>%
      group_by(homeworld) %>%
      mutate(rank = min_rank(desc(mass))) %>%
      collect(),
    starwars
  )
})

test_that("Can mutate after group_by as long as there are no aggregations", {
  skip("group_by not yet implemented: https://github.com/voltrondata/substrait-r/issues/28")

  compare_dplyr_binding(
    .input %>%
      select(int, chr) %>%
      group_by(chr) %>%
      mutate(int = int + 6L) %>%
      collect(),
    tbl
  )
  compare_dplyr_binding(
    .input %>%
      select(mean = int, chr) %>%
      # rename `int` to `mean` and use `mean` in `mutate()` to test that
      # `all_funs()` does not incorrectly identify it as an aggregate function
      group_by(chr) %>%
      mutate(mean = mean + 6L) %>%
      collect(),
    tbl
  )
  expect_warning(
    tbl %>%
      Table$create() %>%
      select(int, chr) %>%
      group_by(chr) %>%
      mutate(avg_int = mean(int)) %>%
      collect(),
    "window functions not currently supported in Arrow; pulling data into R",
    fixed = TRUE
  )
  expect_warning(
    tbl %>%
      Table$create() %>%
      select(mean = int, chr) %>%
      # rename `int` to `mean` and use `mean(mean)` in `mutate()` to test that
      # `all_funs()` detects `mean()` despite the collision with a column name
      group_by(chr) %>%
      mutate(avg_int = mean(mean)) %>%
      collect(),
    "window functions not currently supported in Arrow; pulling data into R",
    fixed = TRUE
  )
})

test_that("Can't just add a vector column with mutate()", {
  skip("https://github.com/voltrondata/substrait-r/issues/63")

  expect_warning(
    expect_equal(
      arrow_substrait_compiler(example_data) %>%
        select(int) %>%
        mutate(again = 1:10) %>%
        collect(),
      tibble(int = example_data$int, again = 1:10)
    ),
    "In again = 1:10, only values of size one are recycled; pulling data into R"
  )
})

test_that("mutate and pmin/pmax", {
  skip("https://github.com/voltrondata/substrait-r/issues/61")

  df <- tibble(
    city = c("Chillan", "Valdivia", "Osorno"),
    val1 = c(200, 300, NA),
    val2 = c(100, NA, NA),
    val3 = c(0, NA, NA)
  )

  compare_dplyr_binding(
    .input %>%
      mutate(
        max_val_1 = pmax(val1, val2, val3),
        max_val_2 = pmax(val1, val2, val3, na.rm = TRUE),
        min_val_1 = pmin(val1, val2, val3),
        min_val_2 = pmin(val1, val2, val3, na.rm = TRUE)
      ) %>%
      collect(),
    df
  )

  compare_dplyr_binding(
    .input %>%
      mutate(
        max_val_1 = pmax(val1 - 100, 200, val1 * 100, na.rm = TRUE),
        min_val_1 = pmin(val1 - 100, 100, val1 * 100, na.rm = TRUE),
      ) %>%
      collect(),
    df
  )
})
