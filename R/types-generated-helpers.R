
create_substrait_message <- function(..., .qualified_name = NULL) {
  structure(list(...), class = "substrait_proto_message")
}

create_substrait_enum <- function(value, .qualified_name) {
  clean_value(value, .qualified_name)
}

unspecified <- function() {
  structure(list(), class = "substrait_proto_unspecified")
}

clean_value <- function(value, .qualified_name) {
  structure(
    list(
      value = value,
      qualified_name = .qualified_name
    ),
    class = "substrait_proto_value"
  )
}
