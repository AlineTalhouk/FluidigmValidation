context("onceT1")

test_that("onceT1 works", {
  expect_equal(onceT1(c("N","T1")),TRUE)
  expect_equal(onceT1(c("N","N","T1","T1","T2")),FALSE)
  expect_equal(onceT1(c("N","T2")),FALSE)
})

test_that("onceT1 throws error with wrong inputs",{
  expect_error(onceT1(1))
  expect_error(onceT1(c("A","T1")))
})
