
test_that("substrait_rel_schema() works for simple relations", {
  df <- data.frame(column1 = double(), column2 = character())

  read_rel <- substrait$ReadRel$create(
    base_schema = as_substrait(df, "substrait.NamedStruct"),
    named_table = substrait$ReadRel$NamedTable$create(
      names = "the_name_of_the_table"
    )
  )

  expect_identical(
    substrait_rel_schema(read_rel),
    substrait$NamedStruct$create(
      names = c("column1", "column2"),
      struct = substrait$Type$Struct$create(
        types = list(
          substrait_fp64(),
          substrait_string()
        )
      )
    )
  )

  rel <- substrait$Rel$create(read = read_rel)
  expect_identical(
    substrait_rel_schema(rel),
    substrait_rel_schema(read_rel)
  )

  filter_rel <- substrait$FilterRel$create(input = rel)
  expect_identical(
    substrait_rel_schema(filter_rel),
    substrait_rel_schema(read_rel)
  )

  sort_rel <- substrait$SortRel$create(input = rel)
  expect_identical(
    substrait_rel_schema(filter_rel),
    substrait_rel_schema(read_rel)
  )

  plan_rel <- substrait$PlanRel$create(rel = rel)
  expect_identical(
    substrait_rel_schema(plan_rel),
    substrait_rel_schema(read_rel)
  )

  expect_snapshot_error(
    substrait_rel_schema(NULL),
  )
})

test_that("substrait_rel_mask() works for simple relations", {
  df <- data.frame(column1 = double(), column2 = character())

  read_rel <- substrait$ReadRel$create(
    base_schema = as_substrait(df, "substrait.NamedStruct"),
    named_table = substrait$ReadRel$NamedTable$create(
      names = "the_name_of_the_table"
    )
  )

  expect_identical(
    substrait_rel_mask(read_rel),
    list(
      column1 = simple_integer_field_reference(0),
      column2 = simple_integer_field_reference(1)
    )
  )

  rel <- substrait$Rel$create(read = read_rel)
  expect_identical(
    substrait_rel_mask(rel),
    substrait_rel_mask(read_rel)
  )

  filter_rel <- substrait$FilterRel$create(input = rel)
  expect_identical(
    substrait_rel_mask(filter_rel),
    substrait_rel_mask(read_rel)
  )

  sort_rel <- substrait$SortRel$create(input = rel)
  expect_identical(
    substrait_rel_mask(filter_rel),
    substrait_rel_mask(read_rel)
  )

  plan_rel <- substrait$PlanRel$create(rel = rel)
  expect_identical(
    substrait_rel_mask(plan_rel),
    substrait_rel_mask(read_rel)
  )

  expect_identical(substrait_rel_mask(NULL), NULL)
})

test_that("rel_tree_modify can modify relation trees", {
  df <- data.frame(column1 = double(), column2 = double())
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
