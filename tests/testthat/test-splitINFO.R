context("splitINFO")

test_that("multiplication works", {
  data("dataForFilterQualityFrequency")
  filtered<-filterQualityFrequency(dataForFilterQualityFrequency,minQual=80,minFreq=0.03)
  expect_error(splitINFO(subset(filtered,select=-INFO)))
  #splitINFO usually should occur after quality and frequency have been filtered
  filtered<-splitINFO(filtered)

  #Check split info processes
  expect_error(splitINFO(subset(filtered,select=-NUMS)))
  expect_equal(nrow(filtered),91)
  expect_equal(filtered$POS[48],133253167)
  expect_equal(filtered$Frequency[91],0.0537)

  #Check by computations
  expect_true(abs(mean(filtered$POS)-60454964.6)<1)
  expect_true(abs(sd(filtered$POS)-54260560.8)<1)
  expect_true(abs(mean(filtered$Frequency)-0.07722637)<0.0001)
  expect_true(abs(sd(filtered$Frequency)-0.11372661)<0.0001)
})
