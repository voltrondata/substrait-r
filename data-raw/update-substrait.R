
curl::curl_download(
  "https://github.com/substrait-io/substrait/archive/refs/heads/main.zip",
  "data-raw/substrait.zip"
)

unzip("data-raw/substrait.zip", exdir = "data-raw")

fs::dir_copy("data-raw/substrait-main", "inst")
fs::file_move("inst/substrait-main", "inst/substrait")

dotfiles <- list.files(
  "inst/substrait", "^\\.",
  all.files = TRUE,
  full.names = TRUE,
  recursive = TRUE
)

unlink(dotfiles)
unlink("inst/substrait/.github", recursive = TRUE)
unlink("inst/substrait/site", recursive = TRUE)

unlink("data-raw/substrait-main", recursive = TRUE)
unlink("data-raw/substrait.zip")

# vendor nanopb
# https://jpa.kapsi.fi/nanopb/download/
curl::curl_download(
  "https://jpa.kapsi.fi/nanopb/download/nanopb-0.4.5-macosx-x86.tar.gz",
  "data-raw/nanopb.tar.gz"
)

untar("data-raw/nanopb.tar.gz", exdir = "data-raw")

nanopb_files <- c(
  "pb_common.c", "pb_common.h",
  "pb_decode.c", "pb_decode.h",
  "pb_encode.c", "pb_encode.h",
  "pb.h"
)

fs::file_copy(file.path("data-raw/nanopb-0.4.5-macosx-x86", nanopb_files), "src")

# generate the protobuf files
proto_files <- list.files(
  "inst/substrait/proto", "\\.proto$",
  recursive = TRUE
)

# have to figure out the correct nanopb.options
# to resolve circular references...-s type:FT_CALLBACK
# is the workaround but this in theory only needs to exist for
# a few fields
proto_files_flat <- paste(proto_files, collapse = " ")

withr::with_dir("inst/substrait/proto", {
  system(
    glue::glue(
      "../../../data-raw/nanopb-0.4.5-macosx-x86/generator-bin/nanopb_generator \\
        -s type:FT_CALLBACK { proto_files_flat } \\
        --output-dir=../../../src"
    )
  )
})

# pull all the .c files out into src/
c_files <- list.files("src", "\\.pb\\.c$", recursive = TRUE, full.names = TRUE)
fs::file_copy(c_files, "src")
