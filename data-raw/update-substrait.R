
curl::curl_download(
  "https://github.com/substrait-io/substrait/archive/refs/tags/v0.6.0.zip",
  "data-raw/substrait.zip"
)

unzip("data-raw/substrait.zip", exdir = "data-raw")

unlink("inst/substrait", recursive = TRUE)
fs::dir_copy("data-raw/substrait-0.6.0", "inst")
fs::file_move("inst/substrait-0.6.0", "inst/substrait")

dotfiles <- list.files(
  "inst/substrait", "^\\.",
  all.files = TRUE,
  full.names = TRUE,
  recursive = TRUE
)

unlink(dotfiles)
unlink("inst/substrait/.github", recursive = TRUE)
unlink("inst/substrait/site", recursive = TRUE)

unlink("data-raw/substrait-0.6.0", recursive = TRUE)
unlink("data-raw/substrait.zip")

# vendor nanopb
# https://jpa.kapsi.fi/nanopb/download/
curl::curl_download(
  "https://jpa.kapsi.fi/nanopb/download/nanopb-0.4.6-macosx-x86.tar.gz",
  "data-raw/nanopb.tar.gz"
)

untar("data-raw/nanopb.tar.gz", exdir = "data-raw")

nanopb_files <- c(
  "pb_common.c", "pb_common.h",
  "pb_decode.c", "pb_decode.h",
  "pb_encode.c", "pb_encode.h",
  "pb.h"
)

unlink(list.files("src", "^pb", full.names = TRUE))
fs::file_copy(file.path("data-raw/nanopb-0.4.6-macosx-x86", nanopb_files), "src")

# add the 'Any' and 'Empty' type
# Probably don't need this because it's built in to Google protobuf
# but I haven't figured out the right includes yet to make it work
# https://github.com/protocolbuffers/protobuf/tree/main/src/google/protobuf
readr::write_file('
syntax = "proto3";

package substrait;

message Any {
  string type_url = 1;
  bytes value = 2;
}

message Empty {}

', "inst/substrait/proto/substrait/any.proto")

# modify references to the Any type and
# clean up reserved words that were used as field names and give them
# an underscore suffix (because nanopb doesn't do any escaping itself)
proto_files <- list.files(
  "inst/substrait/proto", "\\.proto$",
  recursive = TRUE
)

for (f in file.path("inst/substrait/proto", proto_files)) {
  readr::read_file(f) |>
    stringr::str_replace_all(stringr::fixed("google.protobuf.Any"), "Any") |>
    stringr::str_replace_all(stringr::fixed("google.protobuf.Empty"), "Any") |>
    stringr::str_replace_all(
      stringr::fixed("google/protobuf/any.proto"),
      "substrait/any.proto"
    ) |>
    stringr::str_replace_all(
      "(\\s+)(enum|struct|if|else|bool|function)(\\s*=\\s*[0-9]+;)",
      "\\1\\2_\\3"
    ) |>
    readr::write_file(f)
}

# clean up previous nano-pb generated files
unlink(list.files("src", "\\.pb\\.c$", recursive = TRUE, full.names = TRUE))
unlink("src/substrait", recursive = TRUE)

# TODO(dd): figure out the correct nanopb.options
# to resolve circular references...-s type:FT_POINTER
# is the workaround but this in theory only needs to exist for
# message fields. The right way to do this is to configure
# a standalone nanopb substrait project that handles this
# and adds the correct options to the .proto files
# as documented. Currrently we just need the files to exist
# because we parse bits of them to get properties that
# RProtoBut doesn't let us access.
# https://github.com/metormote/nanopb/blob/master/docs/concepts.rst#compiling-proto-files-with-nanopb-options
proto_files_flat <- paste(proto_files, collapse = " ")

withr::with_dir("inst/substrait/proto", {
  system(
    glue::glue(
      "../../../data-raw/nanopb-0.4.6-macosx-x86/generator-bin/nanopb_generator \\
        -s type:FT_POINTER { proto_files_flat } \\
        --output-dir=../../../src"
    )
  )
})

# pull all the .c files out into src/
c_files <- list.files("src", "\\.pb\\.c$", recursive = TRUE, full.names = TRUE)
fs::file_move(c_files, "src")
