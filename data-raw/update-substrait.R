
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

# clean up previous files
unlink(list.files("src", "\\.cc$", full.names = TRUE))
unlink("src/substrait", recursive = TRUE)

# generate the protobuf files
proto_files <- list.files(
  "inst/substrait/proto/substrait/", "\\.proto$",
  full.names = TRUE, recursive = TRUE
)

# needs 'protoc' installed (e.g., brew install protoc)
system(glue::glue("protoc --proto_path=inst/substrait/proto { paste(proto_files, collapse = ' ') } --cpp_out=src"))

# it's slightly easier to pull out the .cc files into the main src/ directory
cc_files <- list.files("src/substrait", "\\.cc$", full.names = TRUE, recursive = TRUE)
fs::file_move(cc_files, "src")


