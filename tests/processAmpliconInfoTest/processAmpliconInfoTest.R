#' Title unit test for FluidigmValidation::processAmpliconInfo
#'
#' @return
#' @export
#'
#' @examples
test.processAmpliconInfo<-function(){
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
  processed<-as.data.frame(t(processAmpliconInfo(ampliconInfo,data)))
  processed<-processed[order(processed$Region),]
  checkTrue(sum(!processed[!is.na(processed$Depth),]$Region==sort(c("YVR","PHX","ATL","HKT","HKG","YUL","CAN","NKG","XMN","SIN","SEA","SJC","SJB")))==0)
  processed_notNA<-processed[!is.na(processed$Depth),]
  checkTrue(processed_notNA[processed_notNA$Region=="NKG",]$Depth==122)
  checkTrue(processed_notNA[processed_notNA$Region=="YUL",]$Depth=="119,148")
  checkException(processAmpliconInfo(3,data))
  checkException(processAmpliconInfo(ampliconInfo,"123"))
  checkException(processAmpliconInfo(subset(ampliconInfo,select=-Position),data))
  checkException(processAmpliconInfo(ampliconInfo,subset(data,select=-POS)))
}
