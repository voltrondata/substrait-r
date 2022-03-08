
test_that("substrait_colnames() works for simple relations", {
  df <- data.frame(column1 = double(), column2 = double())

  read_rel <- substrait$ReadRel$create(
    base_schema = as_substrait(df, "substrait.NamedStruct"),
    named_table = substrait$ReadRel$NamedTable$create(
      names = "the_name_of_the_table"
    )
  )

  expect_identical(substrait_colnames(read_rel), c("column1", "column2"))

  rel <- substrait$Rel$create(read = read_rel)
  expect_identical(substrait_colnames(rel), c("column1", "column2"))

  plan_rel <- substrait$PlanRel$create(rel = rel)
  expect_identical(substrait_colnames(plan_rel), c("column1", "column2"))
})

test_that("rel_tree_modify can modify relation trees", {
  plan <- substrait$Plan$create(
    relations = list(
      substrait$PlanRel$create(
        rel = substrait$Rel$create(
          read = substrait$ReadRel$create(
            base_schema = as_substrait(df, "substrait.NamedStruct"),
            named_table = substrait$ReadRel$NamedTable$create(
              names = "the_name_of_the_table"
            )
          )
        )
      )
    )
  )

  expect_identical(
    rel_tree_modify(plan, character(), function(x) NULL),
    plan
  )

  expect_identical(
    rel_tree_modify(plan, "substrait_PlanRel", function(x) substrait$PlanRel$create()),
    substrait$Plan$create(relations = list(substrait$PlanRel$create()))
  )
})
