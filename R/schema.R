substrait_schema <- function(x){
  UseMethod("substrait_schema", x)
}

substrait_schema.data.frame <- function(x){
  as_substrait(x[0,], "substrait.NamedStruct")
}


