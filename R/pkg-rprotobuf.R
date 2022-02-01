
as_rprotobuf <- function(x) {
  .qualified_name <- gsub("_", ".", class(x)[1])
  descriptor <- RProtoBuf::P(.qualified_name)
  descriptor$read(unclass(x))
}

#' @export
as_substrait.Message <- function(x, .qualified_name = NULL, ...) {
  content <- x$serialize(NULL)
  descriptor <- x$descriptor()
  nesting <- rprotobuf_descriptor_to_class(descriptor)

  stopifnot(
    is.null(.qualified_name) ||
      identical(.qualified_name, paste(c("substrait", nesting), collapse = "."))
  )

  structure(
    content,
    class = c(
      paste(c("substrait", nesting), collapse = "_"),
      "substrait_proto_message",
      "substrait_proto"
    )
  )
}

rprotobuf_descriptor_to_class <- function(descriptor, child = c()) {
  containing <- descriptor$containing_type()
  if (is.null(containing)) {
    c(descriptor$name(), child)
  } else {
    rprotobuf_descriptor_to_class(containing, child = c(descriptor$name(), child))
  }
}