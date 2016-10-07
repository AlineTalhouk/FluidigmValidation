context("filterPassDepth")


test_that("filterPassDepth works", {
  data("PAT37DNA48OPC3S19")
  pat37<-PAT37DNA48OPC3S19
  myMinDepth<-1820
  pat37_filtered<-filterPassDepth(pat37,myMinDepth)
  expect_equal(nrow(pat37_filtered),98)
  expect_true(abs(mean(pat37_filtered$POS)-72754391.8)<1)
  expect_true(abs(sd(pat37_filtered$POS)-56691671.4)<1)
  expect_true(abs(mean(pat37_filtered$DEPTH)-4788.44898)<1)
  expect_true(abs(sd(pat37_filtered$DEPTH)-1525.05233)<1)

})

test_that("Check filterPassDepth throws error with wrong inputs",{
  data("PAT37DNA48OPC3S19")
  pat37<-PAT37DNA48OPC3S19
  expect_error(filterPassDepth("data",15))
  expect_error(filterPassDepth(data=12,"depth"))
  expect_error(filterPassDepth(subset(pat37,select=-QUAL)))
  expect_error(filterPassDepth(subset(pat37,select=-DEPTH)))
  expect_error(filterPassDepth(subset(pat37,select=-FILTER)))
})
