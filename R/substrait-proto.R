
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

create_substrait_enum <- function(value, .qualified_name) {
  descriptor <- RProtoBuf::P(.qualified_name)

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

  if (identical(type, "TYPE_ENUM")) {
    return(create_substrait_enum(value, .qualified_name))
  }

  # eventually this should validate the value in some way
  value
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
  pb_value <- descriptor$value(number = unclass(x))

  print(pb_value, ...)
  cat(pb_value$toString())

  invisible(x)
}
