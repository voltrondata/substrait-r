# Build aggregations
# If we want to do e.g. `df %>% group_by %>% summarise` then the groupings var comes into it
build_aggregations <- function(df, aggregations, groupings = NULL, compiler) {

  context <- new_context(df)
  #compiler <- substrait_compiler()

  # we wanna build measures
  # what are measures?
  # a measure contains individual measure objects which could contain:
  # * a function reference, args (field reference to apply the function to), a phase, and an output type

  substrait$AggregateRel$create(
    measures = list(
      substrait$AggregateRel$Measure$create(
      measure = as_substrait(quo(max(hp)), .ptype = "substrait.AggregateFunction", compiler = compiler, context = context)
      )

    )
  )

}
