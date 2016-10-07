context("twiceT2")


test_that("twiceT2 works", {
  expect_equal(twiceT2(c("N","T2")),FALSE)
  expect_equal(twiceT2(c("N","T2","T1","T2")),TRUE)
  expect_equal(twiceT2(c("N","T2")),FALSE)
})

test_that("twiceT2 throws error with wrong inputs",{
  expect_error(twiceT2(1))
  expect_error(twiceT2(c("A","T2","T2")))
})
