context("processAmpliconInfo")

#Create ampliconInfo
Region<-as.factor(c("YVR","YYZ","YUL","YYC","PEK","NKG","DLC","PVG","SHA","CAN","SZX","HKT","HKG","XMN","CTU","LHR","SIN","PKT","MNL","DVO","CEB",
                    "SEA","SJC","SJB","ATL","PHX","PDX"))
Chrom<-as.factor(c(rep("chr1",4),rep("chr2",8),rep("chr3",3),"chr4",rep("chr5",5),rep("chr6",6)))
ampliconPosition<-10:36
ampliconPosition[8]<-17.5
ampliconPosition[27]<-36.2
ampliconInfo<-data.frame(Region,Chrom,Position=ampliconPosition)

#Create data
POS<-c(10,35,34,67,21,22,11,14,12,19,100,15,17,23,26,31,29,51,43,99,88,77,47,32,33,36,96,92,12)
CHROM<-c("chr1","chr6","chr6","chr1","chr2","chr3","chr8","chr8","chr1","chr2","chr3","chr2","chr2","chr3","chr5","chr6","chr8","chr2","chr1","chr9",
         "chr8","chr6","chr4","chr6","chr6","chr6","chr1","chr1","chr1")
DEPTH<-c(111:131,141:148)
data<-data.frame(POS,CHROM,DEPTH)
data$CHROM<-as.character(data$CHROM)

test_that("processAmpliconInfo works", {
  processed<-as.data.frame(t(processAmpliconInfo(ampliconInfo,data)))
  processed<-processed[order(processed$Region),]
  expect_equal(sum(sort(as.character(processed[processed$Depth!="-1",]$Region))!=sort(c("YVR","PHX","ATL","HKT","HKG","YUL","CAN","NKG","XMN","SIN","SEA","SJC","SJB"))),0)
  expect_true(sum(!processed[processed$Depth!="-1",]$Region==sort(c("YVR","PHX","ATL","HKT","HKG","YUL","CAN","NKG","XMN","SIN","SEA","SJC","SJB")))==0)
  processed_notNeg<-processed[processed$Depth!="-1",]
  expect_true(processed_notNeg[processed_notNeg$Region=="NKG",]$Depth=="122")
  expect_true(processed_notNeg[processed_notNeg$Region=="YUL",]$Depth=="119,148")
})

test_that("processAmpliconInfo throws error with wrong inputs",{
  expect_error(processAmpliconInfo(3,data))
  expect_error(processAmpliconInfo(ampliconInfo,"123"))
  expect_error(processAmpliconInfo(subset(ampliconInfo,select=-Position),data))
  expect_error(processAmpliconInfo(ampliconInfo,subset(data,select=-POS)))
})
