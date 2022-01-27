
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
