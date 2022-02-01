
#' Create 'Substrait' objects
#'
#' @param .qualified_name The fully qualified name of the message type
#'   or enum (e.g., "substrait.Type.Boolean")
#' @param ... Arguments passed to the constructor.
#'
#' @return An object of class "substrait_proto".
#' @export
#'
#' @examples
#' substrait_create("substrait.Type.Boolean", 1, 2)
#' substrait$Type$Boolean$create(1, 2)
#'
substrait_create <- function(.qualified_name, ...) {
  stopifnot(is.character(.qualified_name), length(.qualified_name) == 1)
  parts <- strsplit(.qualified_name, ".", fixed = TRUE)[[1]]

  # This bit of indirection is to get a nice stack trace when an error
  # is thrown and to support tidy dots in ... (might not be necessary).
  expr <- substrait_create_constructor_expr(c(parts, "create"))
  call <- rlang::call2(expr, ...)
  rlang::eval_tidy(call, env = parent.frame())
}


#' Convert to and from 'Substrait' messages
#'
#' @param x An object to convert to or from a 'Substrait' message.
#'   Note that both `as_substrait()` and `from_substrait()` dispatch
#'   on `x`.
#' @param msg A substrait message (e.g., created using [substrait_create()]).
#' @param ... Passed to S3 methods
#' @inheritParams substrait_create
#'
#' @return An RProtoBuf::Message or substrait_proto_message (e.g.,
#'   created by [substrait_create()])
#' @export
#'
#' @examples
#' as_substrait(
#'   list(type_variation_reference = 1, nullability = 2),
#'   "substrait.Type.Boolean"
#' )
#'
as_substrait <- function(x, .qualified_name, ...) {
  UseMethod("as_substrait")
}

#' @rdname as_substrait
#' @export
from_substrait <- function(msg, x, ...) {
  stopifnot(inherits(msg, "substrait_proto"))
  UseMethod("from_substrait", x)
}

#' @export
as_substrait.default <- function(x, .qualified_name = NULL, ...) {
  stop(
    sprintf(
      "Can't create %s from object of type '%s'",
      .qualified_name,
      paste(class(x), collapse = " / ")
    )
  )
}

#' @export
as_substrait.list <- function(x, .qualified_name = NULL, ...) {
  create_substrait_message(!!! x, .qualified_name = .qualified_name)
}

#' @export
from_substrait.list <- function(msg, x, ..., recursive = FALSE) {
  .qualified_name <- gsub("_", ".", class(msg)[1])
  descriptor <- RProtoBuf::P(.qualified_name)
  pb_message <- descriptor$read(unclass(msg))

  msg_names <- names(pb_message)
  msg_names <- msg_names[vapply(msg_names, pb_message$has, logical(1))]
  out <- lapply(msg_names, function(e) pb_message[[e]])
  names(out) <- msg_names

  is_message <- vapply(out, inherits, logical(1), "Message")
  out[is_message] <- lapply(out[is_message], as_substrait)

  if (recursive) {
    out[is_message] <- lapply(
      out[is_message],
      from_substrait.list,
      list(),
      recursive = TRUE
    )
  }

  out
}

substrait_create_constructor_expr <- function(item) {
  if (length(item) == 1) {
    call("::", as.symbol("substrait"), as.symbol(item))
  } else {
    call(
      "$",
      substrait_create_constructor_expr(item[-length(item)]),
      as.symbol(item[length(item)])
    )
  }
}

# The above functions should be the entry point to creating these objects
# to other code in this package. The below functions are internal and
# designed to make the generated code in types-generated.R work. The idea
# is that creating objects using `substrait$Something$create()` and
# `substrait_create("substrait.Something", ...)` will both be stable regardless
# of the backend used to serialize and deserialize protobufs.
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

clean_value <- function(value, type, .qualified_name, repeated = FALSE,
                        call_as_substrait = TRUE) {
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
      } else if (call_as_substrait) {
        clean_value(
          as_substrait(value, .qualified_name),
          type,
          .qualified_name,
          call_as_substrait = FALSE
        )
      } else {
        stop(
          "as_substrait() did not return an RProtoBuf::Message or substrait_proto_message",
          call. = FALSE
        )
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
