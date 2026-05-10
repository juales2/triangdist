test_that("Error handling works", {
  expect_error(dtriang(
    0.5,
    min = 2,
    max = 1,
    mode = 1.5
  ),
  "min must be strictly less than max")
  expect_error(dtriang(
    0.5,
    min = 1,
    max = 1,
    mode = 1
  ),
  "min must be strictly less than max")
  expect_error(dtriang(
    0.5,
    min = 0,
    max = 2,
    mode = 3
  ),
  "mode must be between min and max")
  expect_error(qtriang(-0.1), "p must be between 0 and 1")
  expect_error(qtriang(1.5), "p must be between 0 and 1")
})

test_that("dtriang calculates density correctly", {
  expect_equal(dtriang(-1), 0)
  expect_equal(dtriang(2), 0)
  expect_equal(dtriang(
    0.25,
    min = 0,
    max = 1,
    mode = 0.5
  ), 1)
  expect_equal(dtriang(
    0.5,
    min = 0,
    max = 1,
    mode = 0.5
  ), 2)
  expect_equal(dtriang(
    0.75,
    min = 0,
    max = 1,
    mode = 0.5
  ), 1)
})

test_that("ptriang calculates cumulative probability correctly", {
  expect_equal(ptriang(-1), 0)
  expect_equal(ptriang(
    0.25,
    min = 0,
    max = 1,
    mode = 0.5
  ), 0.125)
  expect_equal(ptriang(
    0.75,
    min = 0,
    max = 1,
    mode = 0.5
  ), 0.875)
  expect_equal(ptriang(2), 1)
})

test_that("qtriang calculates quantiles correctly", {
  expect_equal(qtriang(
    0.125,
    min = 0,
    max = 1,
    mode = 0.5
  ), 0.25)
  expect_equal(qtriang(
    0.875,
    min = 0,
    max = 1,
    mode = 0.5
  ), 0.75)
})

test_that("rtriang generates correct length vector", {
  set.seed(123)
  res <- rtriang(10)
  expect_length(res, 10)
  expect_true(all(res >= 0 & res <= 1))
})
