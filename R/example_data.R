#' Example data
#'
#' Data for use in examples and tests with multiple different data types
#'
#' Currently generated via:
#' tibble::tibble(
#'  int = c(-.Machine$integer.max, 2L, 3L, NA_integer_, 5L, 6L, 7L, 8L, 9L,
#'  .Machine$integer.max),
#'  dbl = c(-Inf, -.Machine$double.xmax, -.Machine$double.xmin, 0,
#'  .Machine$double.xmin, pi, .Machine$double.xmax, Inf, NaN, NA_real_),
#'  dbl2 = c(-Inf, 5, 5, 5, 5, 5, 5, 5, 5, 5),
#'  lgl = c(NA, TRUE, NA, TRUE, FALSE, FALSE, NA, TRUE, FALSE, TRUE),
#'  false = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
#'  chr = c("a", "b", "c", "d", "e", NA, "g", "h", "i", "j"),
#'  verses = c("Por cada muro, un lamento", "En Jerusalén la dorada",
#'    "Y mil vidas malgastadas", "Por cada mandamiento", "Yo soy polvo de tu viento",
#'    "Y aunque sangro de tu herida", "Y cada piedra querida",
#'    "Guarda mi amor más profundo", "No hay una piedra en el mundo",
#'    "Que valga lo que una vida"),
#'  padded_strings = c(" a ", "  b  ",
#'    "   c   ", "    d    ", "     e     ", "      f      ", "       g       ",
#'    "        h        ", "         i         ", "          j          "),
#'  some_negative = c(-1, 2, -3, NA, -5, 6, -7, 8, -9, 10)#,
#'  # https://github.com/voltrondata/substrait-r/issues/80
#'  # dttm = lubridate::ymd_hms(c(
#'  #   "0000-01-01 00:00:00",
#'  #   "1919-05-29 13:08:55",
#'  #   "1955-06-20 04:10:42",
#'  #   "1973-06-30 11:38:41",
#'  #   "1987-03-29 12:49:47",
#'  #   "1991-06-11 19:07:01",
#'  #   NA_character_,
#'  #   "2017-08-21 18:26:40",
#'  #   "2017-08-21 18:26:40",
#'  #   "9999-12-31 23:59:59"
#'  # ))
#')
#'
#' @docType data
#'
#' @usage data(example_data)
#'
#' @keywords datasets
#'
#'
#' @examples
#' data(example_data)
"example_data"
