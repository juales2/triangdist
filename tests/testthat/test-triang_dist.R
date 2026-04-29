test_that("Error handling works", {
  expect_error(dtriang(0.5, min = 2, max = 1, mode = 1.5), "min cannot be greater than max")
  expect_error(dtriang(0.5, min = 0, max = 2, mode = 3), "mode must be between min and max")
  expect_error(qtriang(-0.1), "p must be between 0 and 1")
  expect_error(qtriang(1.5), "p must be between 0 and 1")
})

test_that("dtriang calculates density correctly", {
  expect_equal(dtriang(-1), 0) # x < min
  expect_equal(dtriang(2), 0)  # x > max
  expect_equal(dtriang(0.25, min=0, max=1, mode=0.5), 1) # x < mode
  expect_equal(dtriang(0.5, min=0, max=1, mode=0.5), 2)  # x == mode
  expect_equal(dtriang(0.75, min=0, max=1, mode=0.5), 1) # x > mode
})

test_that("ptriang calculates cumulative probability correctly", {
  expect_equal(ptriang(-1), 0) # q <= min
  expect_equal(ptriang(0.25, min=0, max=1, mode=0.5), 0.125) # q < mode
  expect_equal(ptriang(0.75, min=0, max=1, mode=0.5), 0.875) # q < max
  expect_equal(ptriang(2), 1) # q >= max
})

test_that("qtriang calculates quantiles correctly", {
  expect_equal(qtriang(0.125, min=0, max=1, mode=0.5), 0.25) # p < fc
  expect_equal(qtriang(0.875, min=0, max=1, mode=0.5), 0.75) # p >= fc
})

test_that("rtriang generates correct length vector", {
  set.seed(123)
  res <- rtriang(10)
  expect_length(res, 10)
  expect_true(all(res >= 0 & res <= 1))
})
