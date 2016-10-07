context("onceT2")

test_that("onceT2 works", {
  expect_equal(onceT2(c("N","T2")),TRUE)
  expect_equal(onceT2(c("N","N","T2","T1","T2")),FALSE)
  expect_equal(onceT2(c("N","T1")),FALSE)
})

test_that("onceT2 throws error with wrong inputs",{
  expect_error(onceT2(1))
  expect_error(onceT2(c("A","T2")))
})
