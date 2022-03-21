test_that("build_plan can build a plan from relation with projections", {
  x <- base_table(mtcars) %>%
    dplyr::select(am, hp)

  plan_out <- build_plan(x)

  expect_named(plan_out, "project")

  expect_s3_class(plan_out[["project"]], "substrait_ProjectRel")

  # Projections
  projections <- plan_out[["project"]][["expressions"]]

  expect_length(projections, 2)

  expect_identical(projections[[1]], simple_integer_field_reference(8L))
  expect_identical(projections[[2]], simple_integer_field_reference(3L))
})

test_that("build_plan does nothing for projection if all cols selected", {
  x <- base_table(mtcars) %>%
    dplyr::select(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb)

  plan_out <- build_plan(x)
  expect_named(plan_out, "read")

  expect_s3_class(plan_out[["read"]], "substrait_ReadRel")
})

test_that("build_plan can build a plan from relation with filters", {
  x <- base_table(mtcars) %>%
    dplyr::select(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb)

  plan_out <- build_plan(x)
  expect_named(plan_out, "read")

  expect_s3_class(plan_out[["read"]], "substrait_ReadRel")
})

test_that("build_plan can build a plan from relation with filters", {
  x <- base_table(mtcars) %>%
    dplyr::select(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb)

  plan_out <- build_plan(x)
  expect_named(plan_out, "read")

  expect_s3_class(plan_out[["read"]], "substrait_ReadRel")
})

test_that("build_plan can build a plan from relation with filters", {
  x <- base_table(mtcars) %>%
    dplyr::select(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb)

  plan_out <- build_plan(x)

  expect_s3_class(plan_out, "substrait_Rel")

  expect_named(plan_out, "read")
})

test_that("build_plan can build a plan from relation with filters", {
  x <- base_table(mtcars) %>%
    dplyr::filter(am != 0)

  plan_out <- build_plan(x)

  expect_named(plan_out, "filter")

  # Filters
  filter <- plan_out[["filter"]][[2]][["scalar_function"]][["args"]]

  expect_identical(
    # am
    filter[[1]],
    simple_integer_field_reference(8L)
  )

  # the 0 from am != 0
  expect_equal(
    filter[[2]],
    substrait$Expression$create(
      literal = substrait$Expression$Literal$create(fp64 = 0)
    )
  )
})

test_that("build_plan can build a plan from sorted relations", {
  x <- base_table(mtcars) %>%
    dplyr::filter(hp > 1) %>%
    dplyr::arrange(hp) %>%
    dplyr::select(am, hp)

  plan_out <- build_plan(x)

  expect_named(plan_out, "sort")
  expect_s3_class(plan_out[["sort"]], "substrait_SortRel")

  expect_identical(
    plan_out[["sort"]][["sorts"]][[1]][["expr"]],
    simple_integer_field_reference(3)
  )
})

test_that("build_plan can build a plan from relation with filters and projections", {
  x <- base_table(mtcars) %>%
    dplyr::filter(hp > 1) %>%
    dplyr::filter(am == 0) %>%
    dplyr::select(am, hp, mpg)

  plan_out <- build_plan(x)

  expect_named(plan_out, "project")

  expect_s3_class(plan_out[["project"]], "substrait_ProjectRel")

  # Projections
  projections <- plan_out[["project"]][["expressions"]]

  expect_length(projections, 3)

  expect_identical(projections[[1]], simple_integer_field_reference(8L))
  expect_identical(projections[[2]], simple_integer_field_reference(3L))
  expect_identical(projections[[3]], simple_integer_field_reference(0L))

  # Filters
  filters <- plan_out[["project"]][["input"]][["filter"]][[2]][["scalar_function"]][["args"]]

  expect_length(filters, 2)

  outer_function_1 <- filters[[1]][["scalar_function"]]

  expect_identical(
    # hp
    outer_function_1[["args"]][[1]],
    simple_integer_field_reference(3L)
  )

  # the 1 from hp > 1
  expect_equal(
    outer_function_1[["args"]][[2]],
    substrait$Expression$create(
      literal = substrait$Expression$Literal$create(fp64 = 1)
    )
  )

  outer_function_2 <- filters[[2]][["scalar_function"]]

  # am field
  expect_identical(
    outer_function_2[["args"]][[1]],
    simple_integer_field_reference(8)
  )

  expect_equal(
    outer_function_2[["args"]][[2]],
    substrait$Expression$create(
      literal = substrait$Expression$Literal$create(fp64 = 0)
    )
  )
})
