context("filterQualityFrequency")


test_that("filterQualityFrequency works", {
  #Load the data. For testing on another computer other than Johnson Liu's CTAGMAC12, please correct the file path below. The file in use is from
  #PAT-ID82-DNA-ID130-Curettage-A2_S9.txt
  #The same file was processed in Excel by hand and saved as "PAT-ID82-DNA-ID130-Curettage-A2_S9.xls" whose filtered sheet is being tested
  data("PAT82DNA130CurettageA2S9")
  dataForFilterQualityFrequency<-PAT82DNA130CurettageA2S9
  filtered<-filterQualityFrequency(dataForFilterQualityFrequency,minQual=80,minFreq=0.03)
  expect_equal(nrow(filtered),127)
  expect_true(abs(mean(filtered$POS)-67607674.3)<1)
  expect_true(abs(sd(filtered$POS)-51675152.1)<1)
  expect_true(abs(sd(filtered$Quality)-0)<=0,0)
  expect_equal(mean(filtered$Quality),100)
  expect_true(abs(mean(filtered$Frequency)-0.08839528)<0.0001)
  expect_true(abs(sd(filtered$Frequency)-0.13701195)<0.0001)
  expect_error(filterQualityFrequency(dataForFilterQualityFrequency,minQual=-1,minFreq=0.03))
  expect_error(filterQualityFrequency(dataForFilterQualityFrequency,minQual=101,minFreq=0.03))
  expect_error(filterQualityFrequency(dataForFilterQualityFrequency,minQual=80,minFreq=-12))
  expect_error(filterQualityFrequency(dataForFilterQualityFrequency,minQual=80,minFreq=1.2))
  expect_error(filterQualityFrequency(subset(dataForFilterQualityFrequency,select=-NUMS),minQual=80,minFreq=0.03))
  expect_error(filterQualityFrequency(subset(dataForFilterQualityFrequency,select=-FILTER),minQual=80,minFreq=0.03))
})

test_that("filterQualityFrequency throws error with wrong inputs",{
  data("PAT82DNA130CurettageA2S9")
  dataForFilterQualityFrequency<-PAT82DNA130CurettageA2S9
  expect_error(filterQualityFrequency(dataForFilterQualityFrequency,minQual=-1,minFreq=0.03))
  expect_error(filterQualityFrequency(dataForFilterQualityFrequency,minQual=101,minFreq=0.03))
  expect_error(filterQualityFrequency(dataForFilterQualityFrequency,minQual=80,minFreq=-12))
  expect_error(filterQualityFrequency(dataForFilterQualityFrequency,minQual=80,minFreq=1.2))
  expect_error(filterQualityFrequency(subset(dataForFilterQualityFrequency,select=-NUMS),minQual=80,minFreq=0.03))
  expect_error(filterQualityFrequency(subset(dataForFilterQualityFrequency,select=-FILTER),minQual=80,minFreq=0.03))
})
