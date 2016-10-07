context("labelTumorType")


test_that("labelTumorType works", {
  #Create data frame for testing
  Name<-c("PAT-ID37-DNA-ID48-OP-C3_S19.txt","PAT-ID37-DNA-ID48-OP-D3_S20.txt","PAT-ID37-DNA-ID49-Curettage-E3_S21.txt","PAT-ID37-DNA-ID49-Curettage-F3_S22.txt","PAT-ID37-DNA-ID50-NORMAL-G3_S23.txt")
  val<-1:5
  normalDf<-FluidigmValidation::labelTumorType(data.frame(FileName=Name,val))
  expect_equal(sum(!normalDf==data.frame(FileName=c("PAT-ID37-DNA-ID48-OP-C3_S19.txt","PAT-ID37-DNA-ID48-OP-D3_S20.txt","PAT-ID37-DNA-ID49-Curettage-E3_S21.txt","PAT-ID37-DNA-ID49-Curettage-F3_S22.txt","PAT-ID37-DNA-ID50-NORMAL-G3_S23.txt")
                                        ,val=1:5
                                        ,TumorType=c("T2","T2","T1","T1","N"))),0)
})

test_that("labelTumorType throws error with wrong inputs",{
  Name<-c("PAT-ID37-DNA-ID48-OPS-C3_S19.txt","PAT-ID37-DNA-ID48-OP-D3_S20.txt","PAT-ID37-DNA-ID49-Curettaga-E3_S21.txt","PAT-ID37-DNA-ID49-Curettage-F3_S22.txt","PAT-ID37-DNA-ID50-NORMAL-G3_S23.txt")
  val<-1:5
  expect_error(labelTumorType(data.frame(FileName=Name,val)))
  Name<-c("PAT-ID37-DNA-ID48-OP","PAT-ID37-DNA-ID48-OP-D3_S20.txt","PAT-ID37-DNA-ID49-Curettage-E3_S21.txt","PAT-ID37-DNA-ID49-Curettage-F3_S22.txt","PAT-ID37-DNA-ID50-NORMAL-G3_S23.txt")
  expect_error(labelTumorType(data.frame(Name,val)))
  expect_error(labelTumorType(1))
  expect_error(labelTumorType("Apple"))
})
