
compare_arrow_dplyr_binding <- function(expr, tbl){

  expr <- rlang::enquo(expr)
  expected <- rlang::eval_tidy(expr, rlang::new_data_mask(rlang::env(.input = tbl)))

  out_substrait <- rlang::eval_tidy(expr, rlang::new_data_mask(rlang::env(.input = arrow_substrait_compiler(tbl))))

  expect_identical(out_substrait, expected)

}

compare_arrow_dplyr_error <- function(expr, tbl, ...) {
  # ensure we have supplied tbl
  force(tbl)

  expr <- rlang::enquo(expr)
  msg <- tryCatch(
    rlang::eval_tidy(expr, rlang::new_data_mask(rlang::env(.input = tbl))),
    error = function(e) {
      msg <- conditionMessage(e)

      if (grepl("Problem while computing", msg[1])) {
        msg <- conditionMessage(e$parent)
      }

      # The error here is of the form:
      #
      # Problem with `filter()` .input `..1`.
      # x object 'b_var' not found
      # ℹ Input `..1` is `chr == b_var`.
      #
      # but what we really care about is the `x` block
      # so (temporarily) let's pull those blocks out when we find them
      pattern <- i18ize_error_messages()

      if (grepl(pattern, msg)) {
        msg <- sub(paste0("^.*(", pattern, ").*$"), "\\1", msg)
      }
      msg
    }
  )
  # make sure msg is a character object (i.e. there has been an error)
  # If it did not error, we would get a data.frame or whatever
  # This expectation will tell us "dplyr on data.frame errored is not TRUE"
  expect_true(identical(typeof(msg), "character"), label = "dplyr on data.frame errored")

  expect_error(
    rlang::eval_tidy(
      expr,
      rlang::new_data_mask(rlang::env(.input = arrow_substrait_compiler(tbl)))
    ),
    msg,
    ...
  )
}

i18ize_error_messages <- function() {
  # Figure out what the error messages will be with this LANGUAGE
  # so that we can look for them
  out <- list(
    obj = tryCatch(eval(parse(text = "X_____X")), error = function(e) conditionMessage(e)),
    fun = tryCatch(eval(parse(text = "X_____X()")), error = function(e) conditionMessage(e))
  )
  paste(purrr::map(out, ~ sub("X_____X", ".*", .)), collapse = "|")
}

with_language <- function(lang, expr) {
  old <- Sys.getenv("LANGUAGE")
  # Check what this message is before changing languages; this will
  # trigger caching the transations if the OS does that (some do).
  # If the OS does cache, then we can't test changing languages safely.
  before <- i18ize_error_messages()
  Sys.setenv(LANGUAGE = lang)
  on.exit({
    Sys.setenv(LANGUAGE = old)
    .cache$i18ized_error_pattern <<- NULL
  })
  if (!identical(before, i18ize_error_messages())) {
    skip(paste("This OS either does not support changing languages to", lang, "or it caches translations"))
  }
  force(expr)
}

