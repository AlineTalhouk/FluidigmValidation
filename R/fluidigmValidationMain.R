#' Title Main function to perform analysis of Fluidigm data sets
#'
#' @return an excel file of data labeled, filtered, and only cases with repetitive pos values
#' @export
#'
#' @examples
fluidigmValidationMain<-function(){
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
  write.xlsx(allData,file="allOutput.xlsx",row.names = FALSE,sheetName = "allOutput")
  allRepetitiveData<-onlyRepetitivePos(allData)
  write.xlsx(allRepetitiveData,file="allOutput.xlsx",row.names = FALSE,sheetName = "allRepetitivePositions",append=TRUE)
  moreFiles(allRepetitiveData)
}
