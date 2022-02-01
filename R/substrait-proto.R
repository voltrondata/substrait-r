
create_substrait_message <- function(..., .qualified_name) {
  lst <- rlang::list2(...)
  lst <- lst[!vapply(lst, inherits, logical(1), "substrait_proto_unspecified")]

  descriptor <- RProtoBuf::P(.qualified_name)
  message <- rlang::exec(descriptor$new, !!! lst)

  structure(
    message$serialize(NULL),
    class = c(
      gsub("\\.", "_", .qualified_name),
      "substrait_proto_message",
      "substrait_proto"
    )
  )
}

create_substrait_enum <- function(value, .qualified_name, descriptor = NULL) {
  descriptor <- descriptor %||% RProtoBuf::P(.qualified_name)

  if (length(value) != 1) {
    result <- vapply(value, create_substrait_enum, integer(1), .qualified_name, descriptor)
    return(
      structure(
        result,
        class = c(
          gsub("\\.", "_", .qualified_name),
          "substrait_proto_enum",
          "substrait_proto"
        )
      )
    )
  }

  if (is.character(value)) {
    pb_value <- descriptor$value(name = value)
  } else if (is.numeric(value)) {
    pb_value <- descriptor$value(number = value)
  } else {
    stop("Expected character identifier or integer for enum value", call. = FALSE)
  }

  if (is.null(pb_value)) {
    stop(
      sprintf(
        "'%s' is not a valid identifier for enum %s",
        value,
        .qualified_name
      ),
      call. = FALSE
    )
  }

  structure(
    pb_value$number(),
    class = c(
      gsub("\\.", "_", .qualified_name),
      "substrait_proto_enum",
      "substrait_proto"
    )
  )
}

unspecified <- function() {
  structure(list(), class = "substrait_proto_unspecified")
}

clean_value <- function(value, type, .qualified_name, repeated = FALSE) {
  if (inherits(value, "substrait_proto_unspecified")) {
    return(value)
  }

  switch(
    type,
    TYPE_ENUM = create_substrait_enum(value, .qualified_name),
    TYPE_MESSAGE = {
      if (repeated) {
        lapply(value, clean_value, type, .qualified_name)
      } else if (inherits(value, "Message")) {
        value
      } else if (inherits(value, "substrait_proto_message")) {
        descriptor <- RProtoBuf::P(.qualified_name)
        return(descriptor$read(unclass(value)))
      } else if (is.list(value)) {
        clean_value(
          create_substrait_message(!!! value, .qualified_name = .qualified_name),
          type,
          .qualified_name
        )
      } else {
        stop(sprintf("Can't create '%s' message from passed value", .qualified_name))
      }
    },
    # eventually this should validate the value in some way...as it is now
    # this will get validated by RProtoBuf in the call to descriptor$new()
    value
  )
}

#' @export
print.substrait_proto_message <- function(x, ...) {
  .qualified_name <- gsub("_", ".", class(x)[1])
  descriptor <- RProtoBuf::P(.qualified_name)
  pb_message <- descriptor$read(unclass(x))

  print(pb_message, ...)
  cat(pb_message$toString())

  invisible(x)
}

#' @export
print.substrait_proto_enum <- function(x, ...) {
  .qualified_name <- gsub("_", ".", class(x)[1])
  descriptor <- RProtoBuf::P(.qualified_name)

  pb_value <- lapply(unclass(x), function(e) descriptor$value(number = e))
  numbers <- vapply(pb_value, function(e) e$number(), integer(1))
  labels <- vapply(pb_value, function(e) e$name(), character(1))

  cat(sprintf("<%s[%d]>\n", .qualified_name, length(pb_value)))
  for (i in seq_along(numbers)) {
    cat(sprintf("- %s = %d\n", labels[i], numbers[i]))
  }

  invisible(x)
}
