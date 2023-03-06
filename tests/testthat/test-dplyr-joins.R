
test_that("mutating joins produce identical results with dplyr and duckdb", {
  skip_if_not(has_duckdb_with_substrait())

  cities <- tibble::tibble(
    city = c("Halifax", "Lancaster", "Chicago"),
    country = c("Canada", "United Kingdom", "United States")
  )

  countries <- tibble::tibble(
    country = c("United States", "Canada", "United Kingdom", "Morroco"),
    continent = c("North America", "North America", "Europe", "Africa")
  )

  compare_dplyr_binding(
    .input %>%
      dplyr::left_join(countries, by = "country") %>%
      dplyr::arrange(city) %>%
      dplyr::collect(),
    engine = "duckdb",
    tbl = cities
  )

  compare_dplyr_binding(
    .input %>%
      dplyr::right_join(countries, by = "country") %>%
      dplyr::arrange(city) %>%
      # DuckDB includes country values from the right hand side but dplyr
      # does not
      dplyr::select(city, continent) %>%
      dplyr::collect(),
    engine = "duckdb",
    tbl = cities
  )

  compare_dplyr_binding(
    .input %>%
      dplyr::inner_join(countries, by = "country") %>%
      dplyr::arrange(city) %>%
      dplyr::collect(),
    engine = "duckdb",
    tbl = cities
  )
})
