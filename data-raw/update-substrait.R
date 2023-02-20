
# Update the substrait .proto files ----
# Requirements: the protoc command-line utility; python3 with pip3 install protobuf

<<<<<<< HEAD
VERSION <- "0.20.0"
=======
substrait_version <- "0.24.0"
dest_path <- paste0("data-raw/substrait-", substrait_version)
>>>>>>> 87dc0b6 (Run update-substrait.R)

curl::curl_download(
  glue::glue("https://github.com/substrait-io/substrait/archive/refs/tags/v{VERSION}.zip"),
  "data-raw/substrait.zip"
)

unzip("data-raw/substrait.zip", exdir = "data-raw")

# We keep a copy of some of google's "well known" proto files because they're
# not supplied to RProtoBuf in all cases. Before removing the substrait
# directory, preserve our copy
google_well_known_types_dir <- tempfile()
dir.create(google_well_known_types_dir)
fs::dir_copy("inst/substrait/proto/google", google_well_known_types_dir)

unlink("inst/substrait", recursive = TRUE)
fs::dir_copy(glue::glue("data-raw/substrait-{VERSION}"), "inst")
fs::file_move(glue::glue("inst/substrait-{VERSION}"), "inst/substrait")

dotfiles <- list.files(
  "inst/substrait", "^\\.",
  all.files = TRUE,
  full.names = TRUE,
  recursive = TRUE
)

unlink(dotfiles)
unlink("inst/substrait/.github", recursive = TRUE)
unlink("inst/substrait/site", recursive = TRUE)

unlink(glue::glue("data-raw/substrait-{VERSION}"), recursive = TRUE)
unlink("data-raw/substrait.zip")

dir.create("data-raw/tmp")
withr::with_dir("data-raw/tmp", {
  system('protoc -I=../../inst/substrait/proto --python_out=. `find ../../inst/substrait/proto -name "*.proto"`')
})

module_names <- list.files("data-raw/tmp", "\\.py$", recursive = TRUE) %>%
  stringr::str_remove("\\.py$") %>%
  stringr::str_replace_all("/", ".")

module_names_code <- glue::glue('
import {module_names}
add_pkg_types({module_names})
') %>%
  paste(collapse = "\n")

list_msg_types_all <- glue::glue('

message_types = []
enum_types = []

def add_nested_types(message_type):
    for nested_message_type_name in message_type.DESCRIPTOR.nested_types_by_name:
        nested_type = getattr(message_type, nested_message_type_name)
        message_types.append(nested_type.DESCRIPTOR.full_name)
        add_nested_types(nested_type)

    for nested_enum_type_name in message_type.DESCRIPTOR.enum_types_by_name:
        enum_type = getattr(message_type, nested_enum_type_name)
        enum_types.append(enum_type.DESCRIPTOR.full_name)

def add_pkg_types(pkg):
    for message_type_name in pkg.DESCRIPTOR.message_types_by_name:
        message_type = getattr(pkg, message_type_name)
        message_types.append(message_type.DESCRIPTOR.full_name)
        add_nested_types(message_type)

    for enum_type_name in pkg.DESCRIPTOR.enum_types_by_name:
        enum_type = getattr(pkg, enum_type_name)
        enum_types.append(enum_type.DESCRIPTOR.full_name)

{module_names_code}

print("\\n".join(message_types))
print("\\n".join(enum_types))

')

readr::write_file(list_msg_types_all, "data-raw/tmp/list_all.py")

output <- withr::with_dir("data-raw/tmp", {
  system("python3 list_all.py", intern = TRUE)
})

readr::write_lines(sort(output), "inst/substrait/types_gen.txt")

unlink("data-raw/tmp", recursive = TRUE)

# Copy back the google well-known proto files
fs::dir_copy(
  list.files(google_well_known_types_dir, full.names = TRUE),
  "inst/substrait/proto"
)
