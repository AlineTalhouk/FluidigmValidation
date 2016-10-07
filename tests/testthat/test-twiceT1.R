context("twiceT1")


test_that("twiceT1 works", {
  expect_equal(twiceT1(c("N","T1")),FALSE)
  expect_equal(twiceT1(c("N","T1","T1","T2")),TRUE)
  expect_equal(twiceT1(c("N","T2")),FALSE)
})

test_that("twiceT1 throws error with wrong inputs",{
  expect_error(twiceT1(1))
  expect_error(twiceT1(c("A","T1","T1")))
})
