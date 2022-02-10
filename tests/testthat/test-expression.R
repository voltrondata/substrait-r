
test_that("quosures can be translated to Expression objects", {

  expect_error(
    as_substrait(rlang::quo(stuff), "not.A.Type"),
    "Can't create not.A.Type"
  )
})

test_that("calls can be translated to Expression objects", {


  expect_error(
    as_substrait(call("something"), "not.A.Type"),
    "Can't create not.A.Type"
  )
})

test_that("symbols can be translated to expression objects", {
  expect_identical(
    as_substrait(as.symbol("sym"), fields = list(sym = "some_field")),
    "some_field"
  )

  expect_identical(
    as_substrait(as.symbol("sym"), env = as.environment(list(sym = 5))),
    as_substrait(5, "substrait.Expression")
  )

  expect_error(
    as_substrait(as.symbol("sym"), "not.A.Type"),
    "Can't create not.A.Type"
  )
})
