context("labelMutationGroup")


test_that("labelMutationGroup works", {
  #File names for testing
  fileBank<-c("PAT-ID37-DNA-ID48-OP-C3_S19.txt","PAT-ID37-DNA-ID-OP-D3_S20.txt","PAT-ID37-DNA-ID49-Curettage-E3_S21.txt","PAT-ID37-DNA-ID49-Curettage-F3_S22.txt",
              "PAT-ID37-DNA-ID50-NORMAL-G3_S23.txt")

  #Establish positions
  case1.pos<-rep(1,2)
  case2.pos<-rep(2,2)
  case3.pos<-rep(3,2)
  case4.pos<-rep(4,3)
  case5.pos<-rep(5,3)
  case6.pos<-rep(6,4)
  case7.pos<-rep(7,2)
  case8.pos<-rep(8,3)
  case9.pos<-rep(9,4)
  case10.pos<-rep(10,4)
  case11.pos<-rep(11,5)
  case12.pos<-rep(12,3)
  case13.pos<-rep(13,2)
  case14.pos<-rep(14,3)
  #Establish tumor types
  case1.tumor<-c("T1","T1")
  case2.tumor<-c("T2","T2")
  case3.tumor<-c("T1","T2")
  case4.tumor<-c("T1","T2","T2")
  case5.tumor<-c("T1","T2","N")
  case6.tumor<-c("T1","T2","T2","N")
  case7.tumor<-c("T1","N")
  case8.tumor<-c("T1","T1","T2")
  case9.tumor<-c("T1","T1","T2","T2")
  case10.tumor<-c("T1","T1","T2","N")
  case11.tumor<-c("T1","T1","T2","T2","N")
  case12.tumor<-c("T1","T1","N")
  case13.tumor<-c("T2","N")
  case14.tumor<-c("T2","T2","N")
  #Establish manual tumor group labelling
  case1.group<-rep("Somatic",2)
  case2.group<-rep("Somatic",2)
  case3.group<-rep("Artifact",2)
  case4.group<-c("Artifact","Somatic","Somatic")
  case5.group<-c("Artifact","Artifact","Normal")
  case6.group<-c("Artifact","Germline","Germline","Normal")
  case7.group<-c("Artifact","Normal")
  case8.group<-c("Somatic","Somatic","Artifact")
  case9.group<-rep("Somatic",4)
  case10.group<-c("Germline","Germline","Artifact","Normal")
  case11.group<-c("Germline","Germline","Germline","Germline","Normal")
  case12.group<-c("Germline","Germline","Normal")
  case13.group<-c("Artifact","Normal")
  case14.group<-c("Germline","Germline","Normal")

  #Establish correct data frame to generate no error
  POS<-c(case1.pos,case2.pos,case3.pos,case4.pos,case5.pos,case6.pos,case7.pos,case8.pos,case9.pos,case10.pos,case11.pos,case12.pos,case13.pos,case14.pos)
  TumorType<-c(case1.tumor,case2.tumor,case3.tumor,case4.tumor,case5.tumor,case6.tumor,case7.tumor,case8.tumor,case9.tumor,case10.tumor,case11.tumor,case12.tumor,case13.tumor,case14.tumor)
  FileName<-sample(fileBank,size=length(POS),replace=TRUE)
  normalDf<-data.frame(FileName,POS,TumorType)
  normalDf$FileName<-as.character(normalDf$FileName)
  normalDf$TumorType<-as.character(normalDf$TumorType)
  normalDf.labelled<-labelMutationGroup(normalDf)
  normalDf.group.expected<-c(case1.group,case2.group,case3.group,case4.group,case5.group,case6.group,case7.group,case8.group,case9.group,case10.group,case11.group,case12.group,case13.group,case14.group)
  expect_equal(sum(!normalDf.labelled$mutationGroup==normalDf.group.expected),0)

  attentionDf<-normalDf
  attentionDf$TumorType[7]<-"T2"
  attentionDf.group.expected<-normalDf.group.expected
  attentionDf.group.expected[7:9]<-rep("Attention",3)
  attentionDf.labelled<-labelMutationGroup(attentionDf)
  expect_equal(sum(!attentionDf.labelled$mutationGroup==attentionDf.group.expected),0)

  wrongDf<-normalDf
  wrongDf$TumorType[7]<-"T3"
  expect_error(labelMutationGroup(wrongDf))

  expect_error(labelMutationGroup(subset(normalDf,select=-POS)))
  expect_error(labelMutationGroup(subset(normalDf,select=-TumorType)))
})
