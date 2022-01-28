
library(tidyverse)
library(RProtoBuf)

proto_files <- list.files(
  system.file("substrait/proto", package = "substrait"),
  "\\.proto$", recursive = TRUE
)

readProtoFiles2(
  proto_files,
  protoPath = system.file("substrait/proto", package = "substrait")
)

# use nanopb defs for now
proto_types_with_nesting <- list.files(
  "src", "\\.pb.h",
  full.names = TRUE,
  recursive = TRUE
) %>%
  map(read_file) %>%
  map(str_match_all, "\\}\\s+substrait_[^;]+") %>%
  unlist() %>%
  str_remove("\\}\\s*") %>%
  str_split("_")

proto_types <- tibble(
  name = map_chr(proto_types_with_nesting, ~.x[length(.x)]),
  name_qualified = map_chr(proto_types_with_nesting, paste, collapse = "."),
  name_nesting = proto_types_with_nesting,
  descriptor = map(name_qualified, RProtoBuf::P),
  is_enum = map_lgl(descriptor, ~is.function(.x$value)),
  parent_name_qualified = map_chr(
    name_nesting,
    ~paste(.x[1:(length(.x) - 1)], collapse = ".")
  )
)

enum_types <- proto_types %>%
  filter(is_enum) %>%
  select(-is_enum) %>%
  mutate(
    values = map(
      descriptor,
      ~tibble(name = names(.x), value = map_int(name, function(n) .x[[n]]))
    )
  ) %>%
  select(-descriptor)


rprotobuf_types <- tibble(
  rprotobuf_name = str_subset(names(asNamespace("RProtoBuf")), "^TYPE_"),
  rprotobuf_value = map_int(rprotobuf_name, ~asNamespace("RProtoBuf")[[.x]]),
  name = rprotobuf_name %>% str_to_lower() %>% str_remove("^type_")
) %>%
  arrange(rprotobuf_value)

resolve_name <- function(f) {
  tryCatch(
    f$message_type()$name(),
    error = function(e) tryCatch(
      f$enum_type()$name(),
      error = function(e) tryCatch(
        rprotobuf_types$name[match()]
      )
    )
  )
}

message_types <- proto_types %>%
  filter(!is_enum) %>%
  select(-is_enum) %>%
  mutate(
    fields = map(descriptor, function(d) {
      field_count <- d$field_count()
      fields <- map(seq_len(field_count), d$field)
      field_type_name <- map_chr(fields, function(f) {
        tryCatch(
          f$message_type()$name(),
          error = function(e) tryCatch(
            f$enum_type()$name(),
            error = function(e) tryCatch(
              f$type(as.string = TRUE)
            )
          )
        )
      })

      tibble(
        field_name = map_chr(fields, ~.$name()),
        field_type_name_qualified = dplyr::coalesce(
          proto_types$name_qualified[
            match(field_type_name, proto_types$name)
          ],
          field_type_name
        ),
        has_default_value = map_lgl(fields, ~.x$has_default_value())
      )
    })
  ) %>%
  select(-descriptor)


generate_tree <- function(qualified_name = "substrait", indent = "") {
  message(glue::glue("Generating '{qualified_name}'"))
  name <- strsplit(qualified_name, "\\.")[[1]]
  name <- name[length(name)]

  enums <- enum_types %>% filter(parent_name_qualified == !! qualified_name)
  enum_text <- map_chr(
    enums$name_qualified,
    generate_tree_enum,
    indent = paste0(indent, "  ")
  )
  enum_text_flat <- paste(enum_text, collapse = ",\n")

  type <- message_types %>%
    filter(name_qualified == !! qualified_name)

  if (nrow(type) > 0) {
    type <- lapply(type, "[[", 1)
    formals <- glue::glue("{ type$fields$field_name } = unspecified()")
    formals_flat <- paste(formals, collapse = ", ")

    sanitizers <- glue::glue(
      '  { type$fields$field_name } = clean_value({ type$fields$field_name }, "{ type$fields$field_type_name_qualified }")'
    )
    sanitizers_flat <- paste(sanitizers, collapse = ",\n")

    constructor <- glue::glue(
'\n{ indent }  create = function({ formals_flat }) create_substrait_message(
{ sanitizers_flat },
  .qualified_name = "{ type$name_qualified }"
)
'
)
    constructor <- glue::as_glue(str_replace_all(constructor, "\n", paste0("\n", indent, "  ")))
  } else {
    constructor <- ""
  }

  sub_types <- message_types %>% filter(parent_name_qualified == !! qualified_name)
  sub_types_chr <- map(sub_types$name_qualified, generate_tree, indent = paste0(indent, "  "))
  sub_types_flat <-  paste(sub_types_chr, collapse = ",\n")

  components <- c(enum_text_flat, sub_types_flat, constructor)
  components_flat <- paste(components[components != ""], collapse = ",\n")

  text <- glue::glue(
'
{ indent }{ name } = list(
{ components_flat }
)
'
  )

  glue::as_glue(str_replace_all(text, "\n", paste0("\n", indent)))
}



generate_tree_enum <- function(qualified_name = "substrait.AggregationPhase", indent = "") {
  enum <- enum_types %>%
    filter(name_qualified == !! qualified_name) %>%
    lapply("[[", 1)

  vals <- glue::glue("  { enum$values$name } = { enum$values$value }")
  vals_flat <- paste(vals, collapse = ",\n")

  text <- glue::glue(
    '
{ indent }{ enum$name } = list(
{ vals_flat },
  create = function(value) create_substrait_enum(
    value,
    .qualified_name = "{ enum$name_qualified }"
  )
)
'
  )

  glue::as_glue(str_replace_all(text, "\n", paste0("\n", indent)))
}

everything <- generate_tree("substrait")
write_file(everything, "R/types-generated.R")



