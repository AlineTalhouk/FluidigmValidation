context("onceN")

test_that("Test onceN works", {
  expect_equal(onceN(c("N","T1")),TRUE)
  expect_equal(onceN(c("N","N","T1")),FALSE) #It should return false if N has twice.
  expect_equal(onceN(c("T1","T2")),FALSE)
})

test_that("Test once N with wrong type of inptu",{
  expect_error(onceN(1))
  expect_error(onceN(c("N","A","T2")))
})
