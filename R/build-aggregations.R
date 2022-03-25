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

      # substrait$AggregateFunction$create(
      #   args = unspecified()
      # )#,
      #substrait$AggregateFunction$create()
    )
  )

  # expressions <- lapply(
  #   mutations,
  #   as_substrait,
  #   .ptype = "substrait.Expression",
  #   compiler = compiler,
  #   context = context
  # )


  expressions

}
