winnie<-function(){
  allRawData<-mergeFile()
  labeledData<-labelNT(allRawData)
  passedData<-filterPASS(labeledData)
  data_INFO_splited<-splitINFO(passedData)
  data_NUMS_splited<-splitNUMS(data_INFO_splited)
  data_QF_filtered<-filterQualityFrequency(data_NUMS_splited)
  pureINFO<-data_QF_filtered$INFO
  colnames(pureINFO)<-c("DP","TI","GI","FC",'EXON')
  pureNUMS<-data_QF_filtered$NUMS
  colnames(pureNUMS)<-c("lala","quality","alleicratio","frequency", "U1","U2","U3")
  allData<-subset(data_QF_filtered,select=-INFO)
  allData<-subset(allData,select=-NUMS)
  allData<-cbind(allData,pureINFO,pureNUMS)
  allData<-onlyRepetitivePos(allData)
  write.xlsx(allData,file="output.xlsx",row.names = FALSE)
}
