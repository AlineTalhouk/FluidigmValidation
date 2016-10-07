context("notMutationGroupException")

## TODO: Rename context
## TODO: Add more tests

test_that("notMutationGroupExcpetion works", {
  expect_equal(notMutationGroupException(c("T1","T1")),TRUE)
  expect_equal(notMutationGroupException(c("T2","T2")),TRUE)
  expect_equal(notMutationGroupException(c("T1","T2")),TRUE)
  expect_equal(notMutationGroupException(c("T1","T2","T2")),TRUE)
  expect_equal(notMutationGroupException(c("T1","T2","N")),TRUE)
  expect_equal(notMutationGroupException(c("T1","T2","T2","N")),TRUE)
  expect_equal(notMutationGroupException(c("T1","N")),TRUE)
  expect_equal(notMutationGroupException(c("T1","T1","T2")),TRUE)
  expect_equal(notMutationGroupException(c("T1","T1","T2","T2")),TRUE)
  expect_equal(notMutationGroupException(c("T1","T1","T2","N")),TRUE)
  expect_equal(notMutationGroupException(c("T1","T1","T2","T2","N")),TRUE)
  expect_equal(notMutationGroupException(c("T1","T1","N")),TRUE)
  expect_equal(notMutationGroupException(c("T2","N")),TRUE)
  expect_equal(notMutationGroupException(c("T2","T2","N")),TRUE)
  expect_equal(notMutationGroupException(c("N","N")),FALSE)
  expect_equal(notMutationGroupException(c("T1","T1","T1","N")),FALSE)
  expect_equal(notMutationGroupException(c("T2","N","T2","T2")),FALSE)
})

test_that("notMuationGroupException throws error with wrong inputs",{
  expect_error(notMutationGroupException(23))
  expect_error(notMutationGroupException(c("A","N","T1","T2")))
})
