
.onLoad <- function(...) {
  proto_files <- list.files(
    system.file("substrait/proto", package = "substrait"),
    "\\.proto$", recursive = TRUE
  )

  RProtoBuf::readProtoFiles2(
    proto_files,
    protoPath = system.file("substrait/proto", package = "substrait")
  )
}
