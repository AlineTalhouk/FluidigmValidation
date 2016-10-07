context("checkFC")


test_that("checkFC works", {
  data("dataForCheckFC")
  #results were comparedd with those obtained by hand manipulation in Excel
  filtered<-filterQualityFrequency(dataForCheckFC,minQual=80,minFreq=0.03)
  filtered<-splitINFO(filtered)
  expect_error(checkFC(subset(filtered,select=-FC)))
  FC_checked<-checkFC(filtered)
  expect_equal(nrow(FC_checked),253)
  expect_true(abs(mean(FC_checked$POS)-49279015)<1)
  expect_true(abs(sd(FC_checked$POS)-51489554)<1)
  expect_true(abs(mean(FC_checked$Frequency)-0.141143)<0.0001)
  expect_true(abs(sd(FC_checked$Frequency)-0.164569)<0.0001)
})
